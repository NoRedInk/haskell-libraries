module MySQL.MySQLParam
  ( MySQLParam (..),
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Database.MySQL.Base as Base
import qualified Prelude

data UnexpectedMySQLValue = UnexpectedMySQLValue
  deriving (Show)

instance Exception.Exception UnexpectedMySQLValue

class MySQLParam a where
  decodeParam :: Base.MySQLValue -> a

instance MySQLParam Int where
  decodeParam (Base.MySQLInt8U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt8 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt32U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt32 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt64U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt64 n) = n
  decodeParam _ = Exception.impureThrow UnexpectedMySQLValue

instance MySQLParam Float where
  decodeParam (Base.MySQLDouble n) = n
  decodeParam (Base.MySQLFloat n) = Prelude.realToFrac n
  decodeParam _ = Exception.impureThrow UnexpectedMySQLValue

instance MySQLParam Text where
  decodeParam (Base.MySQLText n) = n
  decodeParam _ = Exception.impureThrow UnexpectedMySQLValue

instance MySQLParam a => MySQLParam (Maybe a) where
  decodeParam Base.MySQLNull = Nothing
  decodeParam x = Just (decodeParam x)

instance MySQLParam Clock.UTCTime where
  decodeParam (Base.MySQLDateTime n) = LocalTime.localTimeToUTC LocalTime.utc n
  decodeParam _ = Exception.impureThrow UnexpectedMySQLValue
