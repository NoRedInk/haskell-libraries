{-# LANGUAGE QuasiQuotes #-}

module Devel where

import Cherry.Prelude
import Internal.Query
import qualified Prelude

main :: Prelude.IO ()
main = do
  let userId = 12 :: Int
  let username = "tester" :: Text
  let
  let query = [sql|!SELECT first_name FROM monolith.users WHERE id = ${userId} AND username = ${username}|] :: Query (Text)
  Prelude.print (preparedString query)
