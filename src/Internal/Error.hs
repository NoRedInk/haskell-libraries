module Internal.Error
  ( Error (..),
    TimeoutOrigin (..),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Internal.Time as Time
import qualified Log
import qualified Text
import Prelude (Show (show))

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
