module MySQL.MySQLParam
  ( MySQLParam (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Database.MySQL.Base as Base

data UnexpectedMySQLValue = UnexpectedMySQLValue
  deriving (Show)

instance Exception.Exception UnexpectedMySQLValue

class MySQLParam a where
  decodeParam :: Base.MySQLValue -> a

instance MySQLParam Int where
  decodeParam (Base.MySQLInt64 n) = n
  decodeParam _ = Exception.impureThrow UnexpectedMySQLValue

instance MySQLParam a => MySQLParam (Maybe a) where
  decodeParam Base.MySQLNull = Nothing
  decodeParam x = Just (decodeParam x)
