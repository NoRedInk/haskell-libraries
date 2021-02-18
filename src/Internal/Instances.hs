{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Instances where

import qualified Data.Int
import qualified Database.PostgreSQL.Typed.Types as PGTypes
import Prelude (fromIntegral)

-- |
-- The default `Int` type we use in our Haskell code is an `Int64`. This
-- corresponds to a `bigint` in SQL. Most of our MySQL tables use regular
-- `integers` though, which are 32 bits.
--
-- In our Postgres databases we default to using `bigint` for columns, but in
-- our legacy MySQL database we have missed that boat. We'd still like to be
-- able to write our Haskell default Ints to/from MySQL without ceremony, so
-- we add these instances to make it possible.
instance PGTypes.PGColumn "integer" Int where
  pgDecode tid tv =
    let (i :: Data.Int.Int32) = PGTypes.pgDecode tid tv
     in fromIntegral i

instance PGTypes.PGParameter "integer" Int where
  pgEncode tid tv =
    let (i :: Data.Int.Int32) = fromIntegral tv
     in PGTypes.pgEncode tid i

-- |
-- Several monolith tables use smaller int sizes to represent
-- enumerables; this type class allows us to extract them safely
-- and without too much ceremony
instance PGTypes.PGColumn "smallint" Int where
  pgDecode tid tv =
    let (i :: Data.Int.Int16) = PGTypes.pgDecode tid tv
     in fromIntegral i

instance PGTypes.PGParameter "smallint" Int where
  pgEncode tid tv =
    let (i :: Data.Int.Int16) = fromIntegral tv
     in PGTypes.pgEncode tid i

instance PGTypes.PGColumn "smallint" Bool where
  pgDecode tid tv = PGTypes.pgDecode tid tv

instance PGTypes.PGParameter "smallint" Bool where
  pgEncode tid tv = PGTypes.pgEncode tid tv
