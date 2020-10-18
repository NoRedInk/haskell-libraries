module Internal.Error
  ( Error (..),
    TimeoutOrigin (..),
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Text
import qualified Internal.Time as Time
import qualified Log
import NriPrelude
import qualified Text
import Prelude (Show (show))

data Error
  = Timeout TimeoutOrigin Time.Interval
  | UniqueViolation Text
  | Other Text [Log.Context]

instance Show Error where
  show (Timeout _ interval) = "Query timed out after " ++ Data.Text.unpack (Text.fromFloat (Time.seconds interval)) ++ " seconds"
  show (UniqueViolation err) = "Query violated uniqueness constraint: " ++ Data.Text.unpack err
  show (Other msg _) = "Query failed with unexpected error: " ++ Data.Text.unpack msg

instance Exception.Exception Error

data TimeoutOrigin = ClientTimeout | ServerTimeout
  deriving (Show)
