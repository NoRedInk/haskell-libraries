{-# LANGUAGE TemplateHaskell #-}

module Postgres.Enum where

import qualified Data.ByteString.Lazy as BSL
import Data.List (group)
import qualified Data.Text.Encoding as Encoding
import qualified Language.Haskell.TH as TH
import Database.PostgreSQL.Typed.Enum
import Database.PostgreSQL.Typed.Protocol (pgSimpleQuery)
import Database.PostgreSQL.Typed.TH (withTPGConnection)
import Database.PostgreSQL.Typed.Dynamic (pgDecodeRep, PGRepType, PGRep)
import Database.PostgreSQL.Typed.Types (PGType, PGVal, PGParameter, pgEncode, PGColumn, pgDecode)
import Set (Set)
import qualified Set
import qualified Prelude 


quote :: Text -> Text
quote t = "'" ++ t ++ "'"

makeValueList :: List Text -> Text
makeValueList list = 
  "[" ++ Text.join ", " (List.map quote list) ++ "]"

makeConList :: List TH.Name -> Text
makeConList list =
  makeValueList (List.map (Prelude.show >> Text.fromList) list)

findDuplicates :: (Ord a, Eq a) => List a -> List a
findDuplicates list = 
  list
    |> List.sort 
    |> group
    |> List.filterMap 
        (\items -> 
          case items of 
            (item : _ : _) -> 
              Just item

            _ ->
              Nothing
        )

generatePGEnum :: TH.Name -> Text -> [(TH.Name, Text)] -> TH.Q [TH.Dec]
generatePGEnum hsTypeName databaseTypeName mapping = do 
  let hsTypeString = Prelude.show hsTypeName

  info <- TH.reify hsTypeName

  conNames <-
    case info of 
      TH.TyConI (TH.DataD _ _ _ _ constructors _) ->
        constructors
          |> Prelude.traverse (\con -> case con of 
                TH.NormalC name [] ->
                  Prelude.pure name

                TH.NormalC name _ ->
                  Prelude.fail <| "Constructor '" ++ Prelude.show name ++ "' cannot have any arguments in order to be mapped to an enum."

                _ -> 
                  Prelude.fail <| "Data type '" ++ hsTypeString ++ "' must contain only simple constructors with no arguments."
              )

      _ ->
        Prelude.fail <| "The datatype name '" ++ hsTypeString ++ "' must be a data declaration."

  -- Check for duplicate values specified in the mapping
  _ <- 
    case findDuplicates (List.map Tuple.second mapping) of 
      [] -> 
        Prelude.pure () 

      duplicates ->
        Prelude.fail <| "The following values in the mapping are duplicated: " ++ Text.toList (makeValueList duplicates)  

  -- Check for duplicate constructor names specified in the mapping
  _ <-
    case findDuplicates (List.map Tuple.first mapping) of
      [] -> 
        Prelude.pure ()

      duplicates ->
        Prelude.fail <| "The following constructors in the mapping are duplicated: " ++ Text.toList (makeConList duplicates)

  let hsConSet = Set.fromList conNames
  let mappingConSet = Set.fromList (List.map Tuple.first mapping)

  _ <- 
    case (Set.toList (Set.diff hsConSet mappingConSet), Set.toList (Set.diff mappingConSet hsConSet)) of 
      ([], []) -> 
        Prelude.pure ()

      ([], mappingOnlyCons) ->
        Prelude.fail <| "The following names in the mapping are not constructors from the data type '" ++ hsTypeString ++ "': " ++ Text.toList (makeConList mappingOnlyCons)

      (hsOnlyCons, _) -> 
        Prelude.fail <| "The following constructors are not present in the mapping: " ++ Text.toList (makeConList hsOnlyCons)

  (pgEnumValues :: List Text) <-
    TH.runIO <| withTPGConnection (\connection -> do 
      -- Check if the databaseTypeName exists on the PG database and is an enum
      -- See https://www.postgresql.org/docs/current/catalog-pg-type.html
      (_, typeRows) <-
        pgSimpleQuery connection (BSL.fromChunks
          [ "SELECT typtype"
          , " FROM pg_catalog.pg_type"
          , " WHERE typname = '", Encoding.encodeUtf8 databaseTypeName, "'"
          ])

      _ <- case List.map (\[v] -> pgDecodeRep v) typeRows of 
        [] -> 
          Prelude.fail ("Type '" ++ Text.toList databaseTypeName ++ "' does not exist on the database.")
        
        -- 'e' means enum type
        ['e'] -> 
          Prelude.pure ()
        
        _ -> 
          Prelude.fail ("Type '" ++ Text.toList databaseTypeName ++ "' is not an enum type.")

      (_, enumRows) <- 
        pgSimpleQuery connection (BSL.fromChunks
          [ "SELECT enumlabel"
          , " FROM pg_catalog.pg_enum"
          , " WHERE enumtypid = '", Encoding.encodeUtf8 databaseTypeName, "'::regtype"
          , " ORDER BY enumsortorder"
          ])
        
      case List.map (\[value] -> pgDecodeRep value) enumRows of 
        [] -> 
          Prelude.fail ("Enum type '" ++ Text.toList databaseTypeName ++ "' does not contain any values.")

        vs -> 
          Prelude.pure vs
    )

  let pgValuesSet = Set.fromList pgEnumValues
  let mappingValuesSet = Set.fromList (List.map Tuple.second mapping) 

  _ <- 
    case (Set.toList (Set.diff pgValuesSet mappingValuesSet), Set.toList (Set.diff mappingValuesSet pgValuesSet)) of 
      ([], []) -> 
        Prelude.pure ()

      ([], hsOnlyValues) ->
        Prelude.fail <| "The following values of type '" ++ hsTypeString ++ "' are not mapped to values of pg enum type '" ++ Text.toList databaseTypeName ++ "': " ++ Text.toList (makeValueList hsOnlyValues)

      (pgOnlyValues, _) -> 
        Prelude.fail <| "The following values from the pg enum type '" ++ Text.toList databaseTypeName ++ "' are not mapped to values of " ++ hsTypeString ++ ": " ++ Text.toList (makeValueList pgOnlyValues)

  -- Validation passed! Let's generate some instances
  let pgTypeString = TH.LitT (TH.StrTyLit (Text.toList databaseTypeName))

  variable <- TH.newName "x"

  let hsToPg = TH.CaseE (TH.VarE variable) (mapping |> List.map (\(conName, pgValue) -> 
                  TH.Match (TH.ConP conName []) (TH.NormalB <| TH.LitE <| TH.StringL <| Text.toList pgValue) [] 
                ))

  let pgToHs = TH.CaseE (TH.VarE variable) ((mapping |> List.map (\(conName, pgValue) ->
                  TH.Match (TH.LitP <| TH.StringL <| Text.toList pgValue) (TH.NormalB <| TH.ConE conName) []
                )) ++ [TH.Match TH.WildP (TH.NormalB <| TH.VarE 'Prelude.error `TH.AppE` (TH.LitE <| TH.StringL "The impossible happened!") ) [] ] )

  Prelude.pure
    [ -- instance PGType "display_element_type" where
      --   PGVal "display_element_type" = DisplayElementType
      TH.InstanceD Nothing [] (TH.ConT ''PGType `TH.AppT` pgTypeString) 
        [ TH.TySynInstD <| TH.TySynEqn Nothing (TH.AppT (TH.ConT ''PGVal) pgTypeString) (TH.ConT hsTypeName) ] 

      -- instance PGParameter "display_element_type" DisplayElementType where
      --   pgEncode _ x = 
      --     case x of 
      --       Labeled -> "labeled"
      --       Blank -> "blank"
    , TH.InstanceD Nothing [] (TH.ConT ''PGParameter `TH.AppT` pgTypeString `TH.AppT` (TH.ConT hsTypeName))
        [ TH.FunD 'pgEncode [ TH.Clause [TH.WildP, TH.VarP variable] (TH.NormalB hsToPg) [] ] ]

      -- instance PGColumn "display_element_type" DisplayElementType where
      --   pgDecode _ x = 
      --     case x of 
      --       "labeled" -> Labeled
      --       "blank" -> Blank
      --       _ -> Prelude.error "The impossible happened!"
    , TH.InstanceD Nothing [] (TH.ConT ''PGColumn `TH.AppT` pgTypeString `TH.AppT` (TH.ConT hsTypeName))
        [ TH.FunD 'pgDecode [ TH.Clause [TH.WildP, TH.VarP variable] (TH.NormalB pgToHs) [] ] ]

      -- instance PGRep DisplayElementType where
      --   PGRepType DisplayElementType = "display_element_type"
    , TH.InstanceD Nothing [] (TH.ConT ''PGRep `TH.AppT` (TH.ConT hsTypeName))
        [ TH.TySynInstD <| TH.TySynEqn Nothing ((TH.ConT ''PGRepType) `TH.AppT` (TH.ConT hsTypeName)) pgTypeString ] 
    ]