-- |
-- Description : Helpers for running queries.
--
-- This module expose some helpers for running postgresql-typed queries, but for
-- MySQL. They return the correct amount of results in a Servant handler, or throw
-- a Rollbarred error.
module MySQL
  ( -- * Connection
    Connection,
    connection,
    readiness,
    readOnlyConnection,
    ReadOnlyConnection (..),

    -- * Settings
    MySQL.Settings.Settings,
    MySQL.Settings.decoder,

    -- * Querying
    sql,
    doQuery,
    MySQL.Query.Query,
    Error.Error (..),
    -- These type classes are for serializing and deserializing data from the
    -- database.
    --
    -- The intent for the PGColumn instance that `postgresql-typed` gives us
    -- and the `MySQLColumn` instance that we model after it is that it
    -- describes safe conversions of database types into Haskell types. It's
    -- intentionally not a decoder with a failure mode.
    --
    -- If we try to shoehorn decoding operations into it we have to resort to
    -- using functions like `Debug.todo ""` in places where decoding fails.
    -- That's not a great pattern to embrace in our code base.
    --
    -- So to prevent ourselves to go down this road we don't expose
    -- `mysqlDecode`, preventing us from defining custom `PGColumn` instances.
    -- We can derive them on newtypes, which is fine and safe. If we want to
    -- read data from the database and transform it into other data in ways
    -- that can fail we can still do so, but not as part of MySQL parsing
    -- logic.
    MySQL.MySQLColumn.MySQLColumn,
    MySQL.MySQLColumn.decodeParam,
    MySQL.MySQLParameter.MySQLParameter,
    MySQL.MySQLParameter.mysqlEncode,

    -- * Handling transactions
    transaction,

    -- * Helpers for uncommon queries
    unsafeBulkifyInserts,
    onConflictUpdate,
    onDuplicateDoNothing,
    sqlYearly,
    replace,
    lastInsertId,
  )
where

import qualified Internal.Error as Error
import MySQL.Internal
import qualified MySQL.MySQLColumn
import qualified MySQL.MySQLParameter
import qualified MySQL.Query
import qualified MySQL.Settings
