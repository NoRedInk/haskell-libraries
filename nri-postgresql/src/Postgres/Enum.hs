{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Provides a functionality for generating a bridge between a Haskell type and a Postgres enum type using Template Haskell.
--
-- Image that we have an `animal_type` defined on our Postgres database:
-- 
-- > CREATE TYPE schema_name.animal_type AS ENUM ('cat', 'dog', 'snake')
--
-- Then we can define an equivalent type in Haskell like so: 
-- 
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# OPTIONS -Wno-orphans #-}
-- >
-- > import Postgres.Enum
-- >
-- > data AnimalType
-- >   = Cat
-- >   | Dog
-- >   | Snake
-- >
-- > $(generatePGEnum ''AnimalType "schema_name.animal_type" 
-- >    [ ('Cat, "cat")
-- >    , ('Dog, "dog")
-- >    , ('Snake, "snake")
-- >    ]
-- >  )
--
-- Note: Having the same type name in different schema's might cause overlapping instances in different modules! 
-- (It's not our fault, this is how postgresql-typed works 🤷‍♂️)
module Postgres.Enum (
  generatePGEnum
) where

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
import qualified Control.Exception 

quote :: Prelude.String -> Prelude.String
quote t = "\"" ++ t ++ "\""

findDuplicates :: (Ord a) => List a -> List a
findDuplicates list = 
  list
    |> List.sort 
    |> group
    |> List.filterMap (List.drop 1 >> List.head)

unexpectedValue :: Prelude.String -> a
unexpectedValue value = 
  Prelude.error <| "Unexpected enum value " ++ quote value ++ " encountered. Perhaps your database is out of sync with your compiled executabe?"

-- | Generate the bridge to allow converting back and forth between the given Haskell data type and Postgres enum type.
-- 
-- This is intended to be used in a TemplateHaskell splice and will generate the `PGType`, `PGParameter`, `PGColumn`, and `PGRep` instances.
--
-- `generatePGEnum` will check at compile time that our mapping is one-to-one between the Haskell datatype and Postgres enum type.
generatePGEnum :: TH.Name           -- ^ The name of the Haskell data type
               -> Text              -- ^ The name of the Postgres enum type (requires a schema)
               -> [(TH.Name, Text)] -- ^ A list of mappings between constructors of the datatype and enum values from Postgres
               -> TH.Q [TH.Dec]
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
        Prelude.fail <| "The following values in the mapping are duplicated: " ++ Prelude.show duplicates

  -- Check for duplicate constructor names specified in the mapping
  _ <-
    case findDuplicates (List.map Tuple.first mapping) of
      [] -> 
        Prelude.pure ()

      duplicates ->
        Prelude.fail <| "The following constructors in the mapping are duplicated: " ++ Prelude.show duplicates

  let hsConSet = Set.fromList conNames
  let mappingConSet = Set.fromList (List.map Tuple.first mapping)

  _ <- 
    case (Set.toList (Set.diff hsConSet mappingConSet), Set.toList (Set.diff mappingConSet hsConSet)) of 
      ([], []) -> 
        Prelude.pure ()

      ([], mappingOnlyCons) ->
        Prelude.fail <| "The following names in the mapping are not constructors from the data type " ++ quote hsTypeString ++ ": " ++ Prelude.show mappingOnlyCons

      (hsOnlyCons, _) -> 
        Prelude.fail <| "The following constructors are not present in the mapping: " ++ Prelude.show hsOnlyCons

  (type_schema_name, type_enum_name) <-
        case Text.split "." databaseTypeName of 
          [schema, enum] ->
            Prelude.pure (schema, enum)

          [_] -> 
            Prelude.fail <| "A schema is required for the database enum name (it might be \"public\" if you didn't explicitly set one."

          _ ->
            Prelude.fail <| "Invalid database enum name \"" ++ (Prelude.show databaseTypeName) ++ "\""

  pgDatabase <-
    TH.runIO <| do 
      nriSettings <- Environment.decode Postgres.Settings.decoder
      Prelude.pure (Postgres.Settings.toPGDatabase nriSettings)

  dbConnectDecls <- useTPGDatabase pgDatabase

  -- Note: We make sure to capture IO errors and re-throw them below in the Q monad so that our test framework can capture them
  (maybePGEnumValues :: Prelude.Either Control.Exception.IOException (List Text)) <-
    TH.runIO <| Control.Exception.try <| withTPGConnection (\connection -> do 
      -- Check if the databaseTypeName exists on the PG database and is an enum
      -- See https://www.postgresql.org/docs/current/catalog-pg-type.html
      typeType <-
        pgSimpleQuery connection (BSL.fromChunks
          [ "SELECT typtype"
          , " FROM pg_catalog.pg_type"
          , " JOIN pg_catalog.pg_namespace ON pg_namespace.oid = pg_type.typnamespace"
          , " WHERE pg_type.typname = '", Encoding.encodeUtf8 type_enum_name, "'", " AND pg_namespace.nspname = '", Encoding.encodeUtf8 type_schema_name, "'"
          ])
          |> Prelude.fmap (\(_, rows) ->
                rows
                  |> List.filterMap (\cols ->
                      case cols of 
                        [enumlabel] ->
                          Just (pgDecodeRep enumlabel)

                        _ -> 
                          Nothing   
                    )
          )

      _ <- case typeType of 
        [] -> 
          Prelude.fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " does not exist on the database.")
        
        -- 'e' means enum type
        ['e'] -> 
          Prelude.pure ()
        
        _ -> 
          Prelude.fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " is not an enum type.")

      enumLabels <- 
        pgSimpleQuery connection (BSL.fromChunks
          [ "SELECT enumlabel"
          , " FROM pg_catalog.pg_enum"
          , " WHERE enumtypid = '", Encoding.encodeUtf8 databaseTypeName, "'::regtype"
          , " ORDER BY enumsortorder"
          ])
          |> Prelude.fmap (\(_, rows) ->
                rows
                  |> List.filterMap (\cols ->
                      case cols of 
                        [enumlabel] ->
                          Just (pgDecodeRep enumlabel)

                        _ -> 
                          Nothing   
                    )
          )
        
      case enumLabels of 
        [] -> 
          Prelude.fail ("Enum type " ++ quote (Text.toList databaseTypeName) ++ " does not contain any values.")

        vs -> 
          Prelude.pure vs
    )

  (pgEnumValues :: List Text) <-
    case maybePGEnumValues of 
      Prelude.Left error -> 
        Prelude.fail (Prelude.show error)
      Prelude.Right vals ->
        Prelude.pure vals

  let pgValuesSet = Set.fromList pgEnumValues
  let mappingValuesSet = Set.fromList (List.map Tuple.second mapping) 

  _ <- 
    case (Set.toList (Set.diff pgValuesSet mappingValuesSet), Set.toList (Set.diff mappingValuesSet pgValuesSet)) of 
      ([], []) -> 
        Prelude.pure ()

      ([], hsOnlyValues) ->
        Prelude.fail <| "The following values of type " ++ quote hsTypeString ++ " are not mapped to values of pg enum type " ++ quote (Text.toList databaseTypeName) ++ ": " ++ Prelude.show hsOnlyValues

      (pgOnlyValues, _) -> 
        Prelude.fail <| "The following values from the pg enum type " ++ quote (Text.toList databaseTypeName) ++ " are not mapped to values of " ++ quote hsTypeString ++ ": " ++ Prelude.show pgOnlyValues

  -- Validation passed! Let's generate some instances
  -- NOTE: We'd like to use the full "schema.type_name" as the type-level string for our instances, but that's not how postgresql-typed works.  
  -- It just strips the schema off
  let pgTypeString = TH.LitT (TH.StrTyLit (Text.toList type_enum_name))

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