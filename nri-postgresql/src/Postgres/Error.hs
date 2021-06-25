module Postgres.Error
  ( Error (..),
    TimeoutOrigin (..),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Log
import qualified Postgres.Time as Time
import qualified Text
import Prelude (Show (show))

-- | A postgres query might fail with one of these errors.
data Error
  = Timeout TimeoutOrigin Time.Interval
  | UniqueViolation Text
  | Other Text [Log.Context]

instance Show Error where
  show (Timeout _ interval) = "Query timed out after " ++ Text.toList (Text.fromFloat (Time.seconds interval)) ++ " seconds"
  show (UniqueViolation err) = "Query violated uniqueness constraint: " ++ Text.toList err
  show (Other msg _) = "Query failed with unexpected error: " ++ Text.toList msg

instance Exception.Exception Error

data TimeoutOrigin = ClientTimeout | ServerTimeout
  deriving (Show)
