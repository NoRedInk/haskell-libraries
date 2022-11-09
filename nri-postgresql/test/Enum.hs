{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}

module Enum where

import Test (Test, describe)
import qualified Database.PostgreSQL.Typed as Core
import Postgres.Enum (generatePGEnum)
import qualified Postgres.TH

{- Set up an enum type on the database at compile time to test with -}
Postgres.TH.useNRIDatabase

[Core.pgSQL| CREATE TYPE test_enum as ENUM ('value_1', 'value_2', 'value_3') |]

data TestEnum
  = Value1
  | Value2
  | Value3

$(generatePGEnum ''TestEnum "test_enum"
    [ ('Value1, "value_1")
    , ('Value2, "value_2")
    , ('Value3, "value_3") 
    ]
 )

tests :: Test
tests = 
  describe
    "Postgres.Settings"
    [ ]