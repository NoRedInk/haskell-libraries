{-# LANGUAGE TypeFamilies #-}

module MySQL.FromRow
  ( FromRow (..),
    CountColumns,
  )
where

import Cherry.Prelude hiding (e)
import qualified Control.Exception.Safe as Exception
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Database.MySQL.Base as Base
import MySQL.MySQLParam (MySQLParam (decodeParam))

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
  CountColumns x = 'SingleColumn

instance (MySQLParam a) => FromRow 'SingleColumn a where
  fromRow _ [x] = decodeParam x
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b
  ) =>
  FromRow 'MultipleColumns (a, b)
  where
  fromRow _ [a, b] =
    ( decodeParam a,
      decodeParam b
    )
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c
  ) =>
  FromRow 'MultipleColumns (a, b, c)
  where
  fromRow _ [a, b, c] = (decodeParam a, decodeParam b, decodeParam c)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d
  ) =>
  FromRow 'MultipleColumns (a, b, c, d)
  where
  fromRow _ [a, b, c, d] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e)
  where
  fromRow _ [a, b, c, d, e] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f)
  where
  fromRow _ [a, b, c, d, e, f] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g)
  where
  fromRow _ [a, b, c, d, e, f, g] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h)
  where
  fromRow _ [a, b, c, d, e, f, g, h] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l,
    MySQLParam m
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l,
    MySQLParam m,
    MySQLParam n
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l,
    MySQLParam m,
    MySQLParam n,
    MySQLParam o
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l,
    MySQLParam m,
    MySQLParam n,
    MySQLParam o,
    MySQLParam p
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns

instance
  ( MySQLParam a,
    MySQLParam b,
    MySQLParam c,
    MySQLParam d,
    MySQLParam e,
    MySQLParam f,
    MySQLParam g,
    MySQLParam h,
    MySQLParam i,
    MySQLParam j,
    MySQLParam k,
    MySQLParam l,
    MySQLParam m,
    MySQLParam n,
    MySQLParam o,
    MySQLParam p,
    MySQLParam q
  ) =>
  FromRow 'MultipleColumns (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  where
  fromRow _ [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] =
    (decodeParam a, decodeParam b, decodeParam c, decodeParam d, decodeParam e, decodeParam f, decodeParam g, decodeParam h, decodeParam i, decodeParam j, decodeParam k, decodeParam l, decodeParam m, decodeParam n, decodeParam o, decodeParam p, decodeParam q)
  fromRow _ _ = Exception.impureThrow UnexpectedAmountOfResultColumns
