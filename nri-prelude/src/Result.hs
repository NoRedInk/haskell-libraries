-- | A @Result@ is the result of a computation that may fail. This is a great
-- way to manage errors in Elm.
module Result
  ( -- * Type and Constructors
    Result (..),

    -- * Mapping
    map,
    map2,
    map3,
    map4,
    map5,

    -- * Chaining
    andThen,

    -- * Handling Errors
    withDefault,
    toMaybe,
    fromMaybe,
    mapError,
  )
where

import Basics
import qualified Internal.Shortcut as Shortcut
import Maybe (Maybe (..))
import Prelude (fmap)
import qualified Prelude

-- | A @Result@ is either @Ok@ meaning the computation succeeded, or it is an
-- @Err@ meaning that there was some failure.
data Result error value
  = Ok value
  | Err error
  deriving (Prelude.Show, Eq)

instance Prelude.Functor (Result error) where
  fmap func result =
    case result of
      Ok value -> Ok (func value)
      Err error -> Err error

instance Prelude.Applicative (Result error) where
  pure = Ok

  (<*>) r1 r2 =
    case (r1, r2) of
      (Ok func, Ok a) -> Ok (func a)
      (Err error, _) -> Err error
      (Ok _, Err error) -> Err error

instance Prelude.Monad (Result error) where
  (>>=) result func =
    case result of
      Ok value -> func value
      Err error -> Err error

-- | If the result is @Ok@ return the value, but if the result is an @Err@ then
-- return a given default value. The following examples try to parse integers.
--
-- > Result.withDefault 0 (Ok 123)   == 123
-- > Result.withDefault 0 (Err "no") == 0
withDefault :: a -> Result b a -> a
withDefault fallback result =
  case result of
    Ok value -> value
    Err _ -> fallback

-- | Apply a function to a result. If the result is @Ok@, it will be converted.
-- If the result is an @Err@, the same error value will propagate through.
--
-- > map sqrt (Ok 4.0)          == Ok 2.0
-- > map sqrt (Err "bad input") == Err "bad input"
map :: (a -> value) -> Result x a -> Result x value
map =
  Shortcut.map

-- | Apply a function if both results are @Ok@. If not, the first @Err@ will
-- propagate through.
--
--    map2 max (Ok 42)   (Ok 13)   == Ok 42
--    map2 max (Err "x") (Ok 13)   == Err "x"
--    map2 max (Ok 42)   (Err "y") == Err "y"
--    map2 max (Err "x") (Err "y") == Err "x"
--
-- This can be useful if you have two computations that may fail, and you want
-- to put them together quickly.
map2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 =
  Shortcut.map2

-- |
map3 :: (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
map3 =
  Shortcut.map3

-- |
map4 :: (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
map4 =
  Shortcut.map4

-- |
map5 :: (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
map5 =
  Shortcut.map5

-- | Chain together a sequence of computations that may fail. It is helpful
-- to see its definition:
--
-- > andThen : (a -> Result e b) -> Result e a -> Result e b
-- > andThen callback result =
-- >     case result of
-- >       Ok value -> callback value
-- >       Err msg -> Err msg
--
-- This means we only continue with the callback if things are going well. For
-- example, say you need to use (@toInt : String -> Result String Int@) to parse
-- a month and make sure it is between 1 and 12:
--
-- > toValidMonth : Int -> Result String Int
-- > toValidMonth month =
-- >     if month >= 1 && month <= 12
-- >         then Ok month
-- >         else Err "months must be between 1 and 12"
--
-- > toMonth : String -> Result String Int
-- > toMonth rawString =
-- >     toInt rawString
-- >       |> andThen toValidMonth
--
-- > -- toMonth "4" == Ok 4
-- > -- toMonth "9" == Ok 9
-- > -- toMonth "a" == Err "cannot parse to an Int"
-- > -- toMonth "0" == Err "months must be between 1 and 12"
--
-- This allows us to come out of a chain of operations with quite a specific error
-- message. It is often best to create a custom type that explicitly represents
-- the exact ways your computation may fail. This way it is easy to handle in your
-- code.
andThen :: (a -> Result c b) -> Result c a -> Result c b
andThen =
  Shortcut.andThen

-- | Transform an @Err@ value. For example, say the errors we get have too much
-- information:
--
-- > parseInt : String -> Result ParseError Int
-- >
-- > type alias ParseError =
-- >     { message : String
-- >     , code : Int
-- >     , position : (Int,Int)
-- >     }
-- >
-- > mapError .message (parseInt "123") == Ok 123
-- > mapError .message (parseInt "abc") == Err "char 'a' is not a number"
mapError :: (a -> b) -> Result a c -> Result b c
mapError func result =
  case result of
    Ok value -> Ok value
    Err error -> Err (func error)

-- | Convert to a simpler @Maybe@ if the actual error message is not needed or
-- you need to interact with some code that primarily uses maybes.
--
-- > parseInt : String -> Result ParseError Int
-- >
-- > maybeParseInt : String -> Maybe Int
-- > maybeParseInt string =
-- >     toMaybe (parseInt string)
toMaybe :: Result a b -> Maybe b
toMaybe result =
  case result of
    Ok value -> Just value
    Err _ -> Nothing

-- | Convert from a simple @Maybe@ to interact with some code that primarily
-- uses @Results@.
--
-- > parseInt : String -> Maybe Int
-- >
-- > resultParseInt : String -> Result String Int
-- > resultParseInt string =
-- >     fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
fromMaybe :: a -> Maybe b -> Result a b
fromMaybe error maybe =
  case maybe of
    Just something -> Ok something
    Nothing -> Err error
