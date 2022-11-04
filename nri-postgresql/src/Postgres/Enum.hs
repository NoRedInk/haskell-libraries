{-# LANGUAGE TemplateHaskell #-}

module Postgres.Enum where

import qualified Data.ByteString.Lazy as BSL
import Data.List (group)
import qualified Data.Text.Encoding as Encoding
import qualified Language.Haskell.TH as TH
import Database.PostgreSQL.Typed.Enum
import Database.PostgreSQL.Typed.Protocol (pgSimpleQuery)
import Database.PostgreSQL.Typed.TH (withTPGConnection)
import Database.PostgreSQL.Typed.Dynamic (pgDecodeRep)
import Set (Set)
import qualified Set
import qualified Prelude 


quote :: Text -> Text
quote t = "'" ++ t ++ "'"

makeValueList :: List Text -> Text
makeValueList list = 
  "[" ++ Text.join ", " (List.map quote list) ++ "]"

generatePGEnum :: TH.Name -> Text -> [(a, Text)] -> TH.Q [TH.Dec]
generatePGEnum name databaseTypeName mapping =
  TH.runIO <| withTPGConnection (\connection -> do
    let hsTypeName = "_"

    let duplicateValues =
          List.map Tuple.second mapping
            |> List.sort 
            |> group
            |> List.filterMap 
                (\items -> case items of 
                            (item : _ : _) -> 
                              Just item

                            _ ->
                              Nothing
                )

    _ <- case duplicateValues of 
      [] -> Prelude.pure () 
      _ -> Prelude.fail <| "The following values in the mapping are duplicated: " ++ Text.toList (makeValueList duplicateValues)

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
        
    (values :: List Text) <- 
      case List.map (\[value] -> pgDecodeRep value) enumRows of 
        [] -> 
          Prelude.fail ("Enum type '" ++ Text.toList databaseTypeName ++ "' does not contain any values.")
        vs -> 
          Prelude.pure vs

    let dbValuesSet = Set.fromList values
    let mappingValuesSet = Set.fromList (List.map Tuple.second mapping)    

    _ <- 
      case (Set.toList (Set.diff dbValuesSet mappingValuesSet), Set.toList (Set.diff mappingValuesSet dbValuesSet)) of 
        ([], []) -> 
          Prelude.pure ()

        ([], hsOnlyValues) ->
          Prelude.fail <| "The following values of type '" ++ hsTypeName ++ "' are not mapped to values of pg enum type '" ++ Text.toList databaseTypeName ++ "': " ++ Text.toList (makeValueList hsOnlyValues)

        (pgOnlyValues, _) -> 
          Prelude.fail <| "The following values from the pg enum type '" ++ Text.toList databaseTypeName ++ "' are not mapped to values of " ++ hsTypeName ++ ": " ++ Text.toList (makeValueList pgOnlyValues)

    Prelude.fail "TODO"
  )