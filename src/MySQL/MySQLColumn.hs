module MySQL.MySQLColumn (MySQLColumn (..)) where

import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString as BS
import qualified Data.Int
import qualified Data.String
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LocalTime
import qualified Database.MySQL.Base as Base
import qualified Prelude

data UnexpectedMySQLValue = UnexpectedMySQLValue Prelude.String Base.MySQLValue

instance Show UnexpectedMySQLValue where
  show (UnexpectedMySQLValue into from) =
    "Unexpected MySQL Value. Don't know how to decode " ++ Prelude.show from ++ " into a " ++ into

instance Exception.Exception UnexpectedMySQLValue

class MySQLColumn a where
  decodeParam :: Base.MySQLValue -> a

instance MySQLColumn Bool where
  decodeParam (Base.MySQLInt8U n) = n > 0
  decodeParam (Base.MySQLInt8 n) = n > 0
  decodeParam (Base.MySQLInt16U n) = n > 0
  decodeParam (Base.MySQLInt16 n) = n > 0
  -- HACK WARNING: We get a Int64 from calculated values in selects
  -- unfortunatelly even if it should be a Int16 that represents a boolean.
  decodeParam (Base.MySQLInt64 n) = n > 0
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "Bool" n)

instance MySQLColumn Int where
  decodeParam (Base.MySQLInt8U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt8 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt32U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt32 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt64U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt64 n) = n
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "Int" n)

instance MySQLColumn Data.Int.Int16 where
  decodeParam (Base.MySQLInt8U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt8 n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16U n) = Prelude.fromIntegral n
  decodeParam (Base.MySQLInt16 n) = n
  -- HACK WARNING: We get a Int64 from calculated values in selects
  -- unfortunatelly even if it should be a Int16 that represents a boolean.
  decodeParam (Base.MySQLInt64 n) = Prelude.fromIntegral n
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "Int16" n)

instance MySQLColumn Float where
  decodeParam (Base.MySQLDouble n) = n
  decodeParam (Base.MySQLFloat n) = Prelude.realToFrac n
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "Float" n)

instance MySQLColumn Text where
  decodeParam (Base.MySQLText n) = n
  decodeParam (Base.MySQLDateTime n) =
    LocalTime.localTimeToUTC LocalTime.utc n
      |> Format.formatTime Format.defaultTimeLocale (Format.iso8601DateFormat (Just "%H:%M:%S"))
      |> Text.fromList
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "Text" n)

instance MySQLColumn BS.ByteString where
  decodeParam (Base.MySQLText n) = Data.String.fromString (Text.toList n)
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "ByteString" n)

instance MySQLColumn a => MySQLColumn (Maybe a) where
  decodeParam Base.MySQLNull = Nothing
  decodeParam x = Just (decodeParam x)

instance MySQLColumn Clock.UTCTime where
  decodeParam (Base.MySQLDateTime n) = LocalTime.localTimeToUTC LocalTime.utc n
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "UTCTime" n)

instance MySQLColumn Calendar.Day where
  decodeParam (Base.MySQLDate n) = n
  decodeParam n = Exception.impureThrow (UnexpectedMySQLValue "UTCTime" n)
