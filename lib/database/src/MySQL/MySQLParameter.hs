module MySQL.MySQLParameter (MySQLParameter (..)) where

import qualified Data.Int
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Word
import qualified Database.MySQL.Base as Base
import qualified Prelude

-- | A type class describing how to encode values for MySQL. The `MySQLValue`
-- type is defined by our MySQL driver library (`mysql-haskell`).
--
-- This is the counterpart of the PGColumn typeclass in `postgresql-typed` for
-- Postgres values.
class MySQLParameter a where
  mysqlEncode :: a -> Base.MySQLValue

instance MySQLParameter Bool where
  mysqlEncode n = Base.MySQLInt8 (if n then 1 else 0)

instance MySQLParameter Data.Int.Int8 where
  mysqlEncode = Base.MySQLInt8

instance MySQLParameter Data.Word.Word16 where
  mysqlEncode = Base.MySQLInt16U

instance MySQLParameter Data.Int.Int16 where
  mysqlEncode = Base.MySQLInt16

instance MySQLParameter Data.Word.Word32 where
  mysqlEncode = Base.MySQLInt32U

instance MySQLParameter Data.Int.Int32 where
  mysqlEncode = Base.MySQLInt32

instance MySQLParameter Data.Word.Word64 where
  mysqlEncode = Base.MySQLInt64U

instance MySQLParameter Int where
  mysqlEncode = Base.MySQLInt64

instance MySQLParameter Prelude.Float where
  mysqlEncode = Base.MySQLFloat

instance MySQLParameter Float where
  mysqlEncode = Base.MySQLDouble

instance MySQLParameter Clock.UTCTime where
  mysqlEncode = Base.MySQLDateTime << LocalTime.utcToLocalTime LocalTime.utc

instance MySQLParameter Calendar.Day where
  mysqlEncode = Base.MySQLDate

instance MySQLParameter Text where
  mysqlEncode = Base.MySQLText

instance MySQLParameter a => MySQLParameter (Maybe a) where
  mysqlEncode Nothing = Base.MySQLNull
  mysqlEncode (Just a) = mysqlEncode a
