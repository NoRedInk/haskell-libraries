{-# LANGUAGE TemplateHaskell #-}

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
module Postgres.Enum
  ( generatePGEnum,
  )
where

import qualified Control.Exception
import qualified Data.ByteString.Lazy as BSL
import Data.List (group)
import qualified Data.Text.Encoding as Encoding
import Database.PostgreSQL.Typed.Array (PGArray, PGArrayType, PGElemType)
import Database.PostgreSQL.Typed.Dynamic (PGRep, PGRepType, pgDecodeRep)
import Database.PostgreSQL.Typed.Protocol (pgSimpleQuery)
import Database.PostgreSQL.Typed.TH (useTPGDatabase, withTPGConnection)
import Database.PostgreSQL.Typed.Types (PGColumn, PGParameter, PGType, PGVal, pgDecode, pgEncode)
import qualified Environment
import qualified Language.Haskell.TH as TH
import qualified Postgres.Settings
import qualified Set
import Prelude (Either (..), String, error, fail, pure, show, traverse)

quote :: String -> String
quote t = "\"" ++ t ++ "\""

findDuplicates :: (Ord a) => List a -> List a
findDuplicates list =
  list
    |> List.sort
    |> group
    |> List.filterMap (List.drop 1 >> List.head)

unexpectedValue :: String -> a
unexpectedValue value =
  error <| "Unexpected enum value " ++ quote value ++ " encountered. Perhaps your database is out of sync with your compiled executabe?"

-- | Generate the bridge to allow converting back and forth between the given Haskell data type and Postgres enum type.
--
-- This is intended to be used in a TemplateHaskell splice and will generate the `PGType`, `PGParameter`, `PGColumn`, and `PGRep` instances.
--
-- `generatePGEnum` will check at compile time that our mapping is one-to-one between the Haskell datatype and Postgres enum type.
generatePGEnum ::
  -- | The name of the Haskell data type
  TH.Name ->
  -- | The name of the Postgres enum type (optionally can have a schema)
  Text ->
  -- | A list of mappings between constructors of the datatype and enum values from Postgres
  [(TH.Name, Text)] ->
  TH.Q [TH.Dec]
generatePGEnum hsTypeName databaseTypeName mapping = do
  let hsTypeString = show hsTypeName

  info <- TH.reify hsTypeName

  conNames <-
    case info of
      TH.TyConI (TH.DataD _ _ _ _ constructors _) ->
        constructors
          |> traverse
            ( \con -> case con of
                TH.NormalC name [] ->
                  pure name
                TH.NormalC name _ ->
                  fail <| "Constructor " ++ quote (show name) ++ " cannot have any arguments in order to be mapped to an enum."
                _ ->
                  fail <| "Data type " ++ quote hsTypeString ++ " must contain only simple constructors with no arguments."
            )
      _ ->
        fail <| "The datatype name " ++ quote hsTypeString ++ " must be a data declaration."

  -- Check for duplicate values specified in the mapping
  _ <-
    case findDuplicates (List.map Tuple.second mapping) of
      [] ->
        pure ()
      duplicates ->
        fail <| "The following values in the mapping are duplicated: " ++ show duplicates

  -- Check for duplicate constructor names specified in the mapping
  _ <-
    case findDuplicates (List.map Tuple.first mapping) of
      [] ->
        pure ()
      duplicates ->
        fail <| "The following constructors in the mapping are duplicated: " ++ show duplicates

  let hsConSet = Set.fromList conNames
  let mappingConSet = Set.fromList (List.map Tuple.first mapping)

  _ <-
    case (Set.toList (Set.diff hsConSet mappingConSet), Set.toList (Set.diff mappingConSet hsConSet)) of
      ([], []) ->
        pure ()
      ([], mappingOnlyCons) ->
        fail <| "The following names in the mapping are not constructors from the data type " ++ quote hsTypeString ++ ": " ++ show mappingOnlyCons
      (hsOnlyCons, _) ->
        fail <| "The following constructors are not present in the mapping: " ++ show hsOnlyCons

  (type_schema_name, type_enum_name) <-
    case Text.split "." databaseTypeName of
      [schema, enum] ->
        pure (schema, enum)
      [enum] ->
        pure ("public", enum)
      _ ->
        fail <| "Invalid database enum name \"" ++ (show databaseTypeName) ++ "\""

  pgDatabase <-
    TH.runIO <| do
      nriSettings <- Environment.decode Postgres.Settings.decoder
      pure (Postgres.Settings.toPGDatabase nriSettings)

  dbConnectDecls <- useTPGDatabase pgDatabase

  -- Note: We make sure to capture IO errors and re-throw them below in the Q monad so that our test framework can capture them
  (maybePGEnumValues :: Either Control.Exception.IOException (List Text)) <-
    TH.runIO
      <| Control.Exception.try
      <| withTPGConnection
        ( \connection -> do
            -- Check if the databaseTypeName exists on the PG database and is an enum
            -- See https://www.postgresql.org/docs/current/catalog-pg-type.html
            typeType <-
              pgSimpleQuery
                connection
                ( BSL.fromChunks
                    [ "SELECT typtype",
                      " FROM pg_catalog.pg_type",
                      " JOIN pg_catalog.pg_namespace ON pg_namespace.oid = pg_type.typnamespace",
                      " WHERE pg_type.typname = '",
                      Encoding.encodeUtf8 type_enum_name,
                      "'",
                      " AND pg_namespace.nspname = '",
                      Encoding.encodeUtf8 type_schema_name,
                      "'"
                    ]
                )
                |> fmap
                  ( \(_, rows) ->
                      rows
                        |> List.filterMap
                          ( \cols ->
                              case cols of
                                [enumlabel] ->
                                  Just (pgDecodeRep enumlabel)
                                _ ->
                                  Nothing
                          )
                  )

            _ <- case typeType of
              [] ->
                fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " does not exist on the database.")
              -- 'e' means enum type
              ['e'] ->
                pure ()
              _ ->
                fail ("Type " ++ quote (Text.toList databaseTypeName) ++ " is not an enum type.")

            enumLabels <-
              pgSimpleQuery
                connection
                ( BSL.fromChunks
                    [ "SELECT enumlabel",
                      " FROM pg_catalog.pg_enum",
                      " WHERE enumtypid = '",
                      Encoding.encodeUtf8 databaseTypeName,
                      "'::regtype",
                      " ORDER BY enumsortorder"
                    ]
                )
                |> fmap
                  ( \(_, rows) ->
                      rows
                        |> List.filterMap
                          ( \cols ->
                              case cols of
                                [enumlabel] ->
                                  Just (pgDecodeRep enumlabel)
                                _ ->
                                  Nothing
                          )
                  )

            case enumLabels of
              [] ->
                fail ("Enum type " ++ quote (Text.toList databaseTypeName) ++ " does not contain any values.")
              vs ->
                pure vs
        )

  (pgEnumValues :: List Text) <-
    case maybePGEnumValues of
      Left err ->
        fail (show err)
      Right vals ->
        pure vals

  let pgValuesSet = Set.fromList pgEnumValues
  let mappingValuesSet = Set.fromList (List.map Tuple.second mapping)

  _ <-
    case (Set.toList (Set.diff pgValuesSet mappingValuesSet), Set.toList (Set.diff mappingValuesSet pgValuesSet)) of
      ([], []) ->
        pure ()
      ([], hsOnlyValues) ->
        fail <| "The following values of type " ++ quote hsTypeString ++ " are not mapped to values of pg enum type " ++ quote (Text.toList databaseTypeName) ++ ": " ++ show hsOnlyValues
      (pgOnlyValues, _) ->
        fail <| "The following values from the pg enum type " ++ quote (Text.toList databaseTypeName) ++ " are not mapped to values of " ++ quote hsTypeString ++ ": " ++ show pgOnlyValues

  -- Validation passed! Let's generate some instances

  -- e.g. "display_element_type" (the type level string)
  let pgTypeString = pure <| TH.LitT (TH.StrTyLit (Text.toList databaseTypeName))
  let pgArrayTypeString = pure <| TH.LitT (TH.StrTyLit (Text.toList databaseTypeName ++ "[]"))

  -- e.g. "DisplayElementType"
  let hsType = pure (TH.ConT hsTypeName)

  varX <- TH.newName "x"
  let varXPattern = pure (TH.VarP varX)

  -- Note: Most of these instance definitions borrowed from https://hackage.haskell.org/package/postgresql-typed-0.6.2.1/docs/src/Database.PostgreSQL.Typed.Enum.html

  -- case x of
  --   Labeled -> "labeled"
  --   Blank -> "blank"
  let hsToPg =
        TH.CaseE
          (TH.VarE varX)
          ( mapping
              |> List.map
                ( \(conName, pgValue) ->
                    TH.Match (TH.ConP conName []) (TH.NormalB <| TH.LitE <| TH.StringL <| Text.toList pgValue) []
                )
          )

  --  case x of
  --    "labeled" -> Labeled
  --    "blank" -> Blank
  --    _ -> unexpectedValue (Prelude.show x)
  let pgToHs =
        TH.CaseE
          (TH.VarE varX)
          ( ( mapping
                |> List.map
                  ( \(conName, pgValue) ->
                      TH.Match (TH.LitP <| TH.StringL <| Text.toList pgValue) (TH.NormalB <| TH.ConE conName) []
                  )
            )
              ++ [TH.Match TH.WildP (TH.NormalB <| TH.AppE (TH.VarE 'unexpectedValue) (TH.AppE (TH.VarE 'show) (TH.VarE varX))) []]
          )

  [d|
    instance PGType $pgTypeString where
      type PGVal $pgTypeString = $hsType

    instance PGParameter $pgTypeString $hsType where
      pgEncode _ $varXPattern =
        $(pure hsToPg)

    instance PGColumn $pgTypeString $hsType where
      pgDecode _ $varXPattern =
        $(pure pgToHs)

    instance PGRep $hsType where
      type PGRepType $hsType = $pgTypeString

    instance PGType $pgArrayTypeString where
      type PGVal $pgArrayTypeString = PGArray $hsType

    instance PGArrayType $pgArrayTypeString where
      type PGElemType $pgArrayTypeString = $pgTypeString
    |]
    |> fmap (dbConnectDecls ++)
