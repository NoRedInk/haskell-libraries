module MySQL.MySQLColumn (MySQLColumn (..)) where

import Cherry.Prelude
import qualified Data.Int
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
class MySQLColumn a where
  mysqlEncode :: a -> Base.MySQLValue

instance MySQLColumn Data.Int.Int8 where
  mysqlEncode = Base.MySQLInt8

instance MySQLColumn Data.Word.Word16 where
  mysqlEncode = Base.MySQLInt16U

instance MySQLColumn Data.Int.Int16 where
  mysqlEncode = Base.MySQLInt16

instance MySQLColumn Data.Word.Word32 where
  mysqlEncode = Base.MySQLInt32U

instance MySQLColumn Data.Int.Int32 where
  mysqlEncode = Base.MySQLInt32

instance MySQLColumn Data.Word.Word64 where
  mysqlEncode = Base.MySQLInt64U

instance MySQLColumn Int where
  mysqlEncode = Base.MySQLInt64

instance MySQLColumn Prelude.Float where
  mysqlEncode = Base.MySQLFloat

instance MySQLColumn Float where
  mysqlEncode = Base.MySQLDouble

instance MySQLColumn Clock.UTCTime where
  mysqlEncode = Base.MySQLDateTime << LocalTime.utcToLocalTime LocalTime.utc

instance MySQLColumn Text where
  mysqlEncode = Base.MySQLText

instance MySQLColumn a => MySQLColumn (Maybe a) where
  mysqlEncode Nothing = Base.MySQLNull
  mysqlEncode (Just a) = mysqlEncode a
