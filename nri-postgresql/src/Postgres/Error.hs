module Postgres.Error
  ( Error (..),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Log
import qualified Text
import Prelude (Show (show))

-- | A postgres query might fail with one of these errors.
data Error
  = Timeout Float
  | UniqueViolation Text
  | Other Text [Log.Context]

instance Show Error where
  show (Timeout interval) = "Query timed out after " ++ Text.toList (Text.fromFloat interval) ++ " milliseconds"
  show (UniqueViolation err) = "Query violated uniqueness constraint: " ++ Text.toList err
  show (Other msg _) = "Query failed with unexpected error: " ++ Text.toList msg

instance Exception.Exception Error
