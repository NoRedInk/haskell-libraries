{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -Wno-orphans #-}

module Postgres.Enum where

import qualified Environment
import qualified Postgres.Settings
import qualified Data.ByteString.Lazy as BSL
import Data.List (group)
import qualified Data.Text.Encoding as Encoding
import qualified Language.Haskell.TH as TH
import Database.PostgreSQL.Typed.Protocol (pgSimpleQuery)
import Database.PostgreSQL.Typed.TH (withTPGConnection, useTPGDatabase)
import Database.PostgreSQL.Typed.Dynamic (pgDecodeRep, PGRepType, PGRep)
import Database.PostgreSQL.Typed.Types (PGType, PGVal, PGParameter, pgEncode, PGColumn, pgDecode)
import qualified Set
import qualified Prelude 

quote :: Prelude.String -> Prelude.String
quote t = "\"" ++ t ++ "\""

makeValueList :: List Text -> Text
makeValueList list = 
  "[" ++ Text.join ", " (List.map (Text.toList >> quote >> Text.fromList) list) ++ "]"

makeConList :: List TH.Name -> Text
makeConList list =
  makeValueList (List.map (Prelude.show >> Text.fromList) list)

findDuplicates :: (Ord a) => List a -> List a
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

unexpectedValue :: Prelude.String -> a
unexpectedValue value = 
  Prelude.error <| "Unexpected enum value " ++ quote value ++ " encountered. Perhaps your database is out of sync with your compiled executabe?"

{- Generate instances to connect a Haskell enum type to a postgres ENUM datatype. 

```
data DisplayElementType
  = Labeled
  | Blank
  deriving (Eq, Show, Ord)

$(generatePGEnum ''DisplayElementType "display_element_type" 
    [ ('Labeled, "labeled")
    , ('Blank, "blank")
    ]
  )
```

Note: You will need to enable the `TemplateHaskell` and `TypeFamilies` extensions and disable the orphan instances warning in the module you call `generatePGEnum` from.  
Add the following to the top of your file:

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}
```
-}
generatePGEnum :: TH.Name -> Text -> [(TH.Name, Text)] -> TH.Q [TH.Dec]
generatePGEnum hsTypeName databaseTypeName mapping = do 
  let hsTypeString = Prelude.show hsTypeName

  typeFamiliesEnabled <- TH.isExtEnabled TH.TypeFamilies

  if not typeFamiliesEnabled then 
    Prelude.fail "You need to enable the `TypeFamilies` extension in this module to automatically derive enum instances. Add {-# LANGUAGE TypeFamilies #-} above the module definition."
  else
    Prelude.pure ()

  info <- TH.reify hsTypeName

  conNames <-
    case info of 
      TH.TyConI (TH.DataD _ _ _ _ constructors _) ->
        constructors
          |> Prelude.traverse (\con -> case con of 
                TH.NormalC name [] ->
                  Prelude.pure name

                TH.NormalC name _ ->
                  Prelude.fail <| "Constructor " ++ quote (Prelude.show name) ++ " cannot have any arguments in order to be mapped to an enum."

                _ -> 
                  Prelude.fail <| "Data type " ++ quote hsTypeString ++ " must contain only simple constructors with no arguments."
              )

      _ ->
        Prelude.fail <| "The datatype name " ++ quote hsTypeString ++ " must be a data declaration."

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
        Prelude.fail <| "The following names in the mapping are not constructors from the data type " ++ quote hsTypeString ++ ": " ++ Text.toList (makeConList mappingOnlyCons)

      (hsOnlyCons, _) -> 
        Prelude.fail <| "The following constructors are not present in the mapping: " ++ Text.toList (makeConList hsOnlyCons)

  pgDatabase <-
    TH.runIO <| do 
      nriSettings <- Environment.decode Postgres.Settings.decoder
      Prelude.pure (Postgres.Settings.toPGDatabase nriSettings)

  dbConnectDecls <- useTPGDatabase pgDatabase

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
          Prelude.fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " does not exist on the database.")
        
        -- 'e' means enum type
        ['e'] -> 
          Prelude.pure ()
        
        _ -> 
          Prelude.fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " is not an enum type.")

      (_, enumRows) <- 
        pgSimpleQuery connection (BSL.fromChunks
          [ "SELECT enumlabel"
          , " FROM pg_catalog.pg_enum"
          , " WHERE enumtypid = '", Encoding.encodeUtf8 databaseTypeName, "'::regtype"
          , " ORDER BY enumsortorder"
          ])
        
      case List.map (\[value] -> pgDecodeRep value) enumRows of 
        [] -> 
          Prelude.fail ("Enum type " ++ quote (Text.toList databaseTypeName) ++ " does not contain any values.")

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
        Prelude.fail <| "The following values of type " ++ quote hsTypeString ++ " are not mapped to values of pg enum type " ++ quote (Text.toList databaseTypeName) ++ ": " ++ Text.toList (makeValueList hsOnlyValues)

      (pgOnlyValues, _) -> 
        Prelude.fail <| "The following values from the pg enum type " ++ quote (Text.toList databaseTypeName) ++ " are not mapped to values of " ++ quote hsTypeString ++ ": " ++ Text.toList (makeValueList pgOnlyValues)

  -- Validation passed! Let's generate some instances
  let pgTypeString = TH.LitT (TH.StrTyLit (Text.toList databaseTypeName))

  variable <- TH.newName "x"

  -- Note: Most of these instance definitions borrowed from https://hackage.haskell.org/package/postgresql-typed-0.6.2.1/docs/src/Database.PostgreSQL.Typed.Enum.html

  -- See definition of PGParameter below
  let hsToPg = TH.CaseE (TH.VarE variable) (mapping |> List.map (\(conName, pgValue) -> 
                  TH.Match (TH.ConP conName []) (TH.NormalB <| TH.LitE <| TH.StringL <| Text.toList pgValue) [] 
                ))

  -- See definition of PGColumn below
  let pgToHs = TH.CaseE (TH.VarE variable) ((mapping |> List.map (\(conName, pgValue) ->
                  TH.Match (TH.LitP <| TH.StringL <| Text.toList pgValue) (TH.NormalB <| TH.ConE conName) []
                )) ++ [TH.Match TH.WildP (TH.NormalB <| TH.AppE (TH.VarE 'unexpectedValue) (TH.AppE (TH.VarE 'Prelude.show) (TH.VarE variable))) [] ] )

  Prelude.pure <|
    dbConnectDecls
    ++
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
      --       _ -> unexpectedValue (Prelude.show x)
    , TH.InstanceD Nothing [] (TH.ConT ''PGColumn `TH.AppT` pgTypeString `TH.AppT` (TH.ConT hsTypeName))
        [ TH.FunD 'pgDecode [ TH.Clause [TH.WildP, TH.VarP variable] (TH.NormalB pgToHs) [] ] ]

      -- instance PGRep DisplayElementType where
      --   PGRepType DisplayElementType = "display_element_type"
    , TH.InstanceD Nothing [] (TH.ConT ''PGRep `TH.AppT` (TH.ConT hsTypeName))
        [ TH.TySynInstD <| TH.TySynEqn Nothing ((TH.ConT ''PGRepType) `TH.AppT` (TH.ConT hsTypeName)) pgTypeString ] 
    ]