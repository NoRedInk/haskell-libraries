{- HLINT ignore -}

module Internal.Shortcut
  ( map,
    map2,
    map3,
    map4,
    map5,
    map6,
    map7,
    map8,
    map9,
    andMap,
    andThen,
    afterwards,
    blank,
  )
where

import Prelude (Applicative, Functor, Monad, fmap, pure, (<*>), (>>=))

andThen :: Monad m => (a -> m b) -> m a -> m b
andThen b a =
  a >>= b

afterwards :: Monad m => m b -> m a -> m b
afterwards b a =
  a >>= (\_ -> b)

map :: Functor m => (a -> value) -> m a -> m value
map =
  fmap

map2 :: Applicative m => (a -> b -> value) -> m a -> m b -> m value
map2 func a b =
  pure func <*> a <*> b

map3 :: Applicative m => (a -> b -> c -> value) -> m a -> m b -> m c -> m value
map3 func a b c =
  pure func <*> a <*> b <*> c

map4 :: Applicative m => (a -> b -> c -> d -> value) -> m a -> m b -> m c -> m d -> m value
map4 func a b c d =
  pure func <*> a <*> b <*> c <*> d

map5 :: Applicative m => (a -> b -> c -> d -> e -> value) -> m a -> m b -> m c -> m d -> m e -> m value
map5 func a b c d e =
  pure func <*> a <*> b <*> c <*> d <*> e

map6 :: Applicative m => (a -> b -> c -> d -> e -> f -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m value
map6 func a b c d e f =
  pure func <*> a <*> b <*> c <*> d <*> e <*> f

map7 :: Applicative m => (a -> b -> c -> d -> e -> f -> g -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m value
map7 func a b c d e f g =
  pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g

map8 :: Applicative m => (a -> b -> c -> d -> e -> f -> g -> h -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m value
map8 func a b c d e f g h =
  pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h

map9 :: Applicative m => (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m i -> m value
map9 func a b c d e f g h i =
  pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i

andMap :: Applicative m => m a -> m (a -> b) -> m b
andMap m mf =
  mf <*> m

blank :: Monad m => m ()
blank =
  pure ()
