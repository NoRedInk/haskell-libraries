{-# LANGUAGE TypeFamilies #-}

module MySQL.FromRow
  ( FromRow (..),
    CountColumns,
  )
where

import qualified Control.Exception.Safe as Exception
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Database.MySQL.Base as Base
import MySQL.MySQLColumn (MySQLColumn (decodeParam))
import NriPrelude hiding (e)

data UnexpectedAmountOfResultColumns = UnexpectedAmountOfResultColumns
  deriving (Show)

instance Exception.Exception UnexpectedAmountOfResultColumns

class FromRow (c :: ColumnCount) a where
  fromRow :: Proxy c -> [Base.MySQLValue] -> a

data ColumnCount = SingleColumn | MultipleColumns

type family CountColumns (c :: Type) :: ColumnCount where
  CountColumns (a, b) = 'MultipleColumns
  CountColumns (a, b, c) = 'MultipleColumns
  CountColumns (a, b, c, d) = 'MultipleColumns
  CountColumns (a, b, c, d, e) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = 'MultipleColumns
  CountColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = 'MultipleColumns
  CountColumns x = 'SingleColumn

instance (MySQLColumn a) => FromRow 'SingleColumn a where
  fromRow _ [x] = decodeParam x
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b
  ) =>
  FromRow 'MultipleColumns (a, b)
  where
  fromRow _ [a, b] =
    ( decodeParam a,
      decodeParam b
    )
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c
  ) =>
  FromRow 'MultipleColumns (a, b, c)
  where
  fromRow _ [a, b, c] = (decodeParam a, decodeParam b, decodeParam c)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d
  ) =>
  FromRow 'MultipleColumns (a, b, c, d)
  where
  fromRow _ [a, b, c, d] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e)
  where
  fromRow _ [a, b, c, d, e] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f)
  where
  fromRow _ [a, b, c, d, e, f] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g)
  where
  fromRow _ [a, b, c, d, e, f, g] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h)
  where
  fromRow _ [a, b, c, d, e, f, g, h] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u,
    MySQLColumn v
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u, decodeParam v)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u,
    MySQLColumn v,
    MySQLColumn w
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u, decodeParam v, decodeParam w)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u,
    MySQLColumn v,
    MySQLColumn w,
    MySQLColumn x
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u, decodeParam v, decodeParam w, decodeParam x)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u,
    MySQLColumn v,
    MySQLColumn w,
    MySQLColumn x,
    MySQLColumn y
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u, decodeParam v, decodeParam w, decodeParam x, decodeParam y)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLColumn a,
    MySQLColumn b,
    MySQLColumn c,
    MySQLColumn d,
    MySQLColumn e,
    MySQLColumn f,
    MySQLColumn g,
    MySQLColumn h,
    MySQLColumn i,
    MySQLColumn j,
    MySQLColumn k,
    MySQLColumn l,
    MySQLColumn m,
    MySQLColumn n,
    MySQLColumn o,
    MySQLColumn p,
    MySQLColumn q,
    MySQLColumn r,
    MySQLColumn s,
    MySQLColumn t,
    MySQLColumn u,
    MySQLColumn v,
    MySQLColumn w,
    MySQLColumn x,
    MySQLColumn y,
    MySQLColumn z
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q, decodeParam r, decodeParam s, decodeParam t, decodeParam u, decodeParam v, decodeParam w, decodeParam x, decodeParam y, decodeParam z)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns
