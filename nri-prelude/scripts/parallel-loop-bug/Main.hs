{-# LANGUAGE DeriveAnyClass #-}

-- | Reproduction for an intermittent `<<loop>>` (NonTermination) crash from
-- `Test.run` under the threaded RTS with multiple capabilities (`+RTS -N`).
--
-- The suite below is a dozen *ungrouped* tests (so `Test.run` executes them in
-- parallel via `Task.parallel`), each decoding a tiny document to a *distinct*
-- type — i.e. each forces a distinct per-type decoder CAF. Looping the binary
-- on a multi-core box crashes with `parallel-loop-bug: <<loop>>` in a small
-- fraction of runs at `-N >= 4`, and never at `-N1`.
--
-- See scripts/parallel-loop-bug/README.md for how to run it and what we know.
module Main (main) where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import qualified Expect
import GHC.Generics (Generic)
import Test (Test, describe, run, test)
import qualified Prelude
import Prelude (Either (..), IO, Int, Maybe, Show)

-- A dozen DISTINCT record types => a dozen distinct FromJSON decoder CAFs.
-- Distinct field names; each field optional so the one document decodes into
-- every type. The decode is small but real (YAML); the trigger is the *variety
-- of distinct decoders forced concurrently*, not how big each document is.
data R1 = R1 {a1 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R2 = R2 {a2 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R3 = R3 {a3 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R4 = R4 {a4 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R5 = R5 {a5 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R6 = R6 {a6 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R7 = R7 {a7 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R8 = R8 {a8 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R9 = R9 {a9 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R10 = R10 {a10 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R11 = R11 {a11 :: Maybe Int} deriving (Show, Generic, FromJSON)
data R12 = R12 {a12 :: Maybe Int} deriving (Show, Generic, FromJSON)

doc :: BS.ByteString
doc = "{}\n"

mk :: (Show a) => Text -> (BS.ByteString -> Either Yaml.ParseException a) -> Test
mk name dec =
  test name (\_ ->
    case dec doc of
      Right x -> Expect.equal (Prelude.length (Prelude.show x)) (Prelude.length (Prelude.show x))
      Left _ -> Expect.fail "decode failed")

main :: IO ()
main =
  run
    (describe
      "parallel-loop-bug"
      [ mk "r1" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R1),
        mk "r2" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R2),
        mk "r3" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R3),
        mk "r4" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R4),
        mk "r5" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R5),
        mk "r6" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R6),
        mk "r7" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R7),
        mk "r8" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R8),
        mk "r9" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R9),
        mk "r10" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R10),
        mk "r11" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R11),
        mk "r12" (Yaml.decodeEither' :: BS.ByteString -> Either Yaml.ParseException R12)
      ])
