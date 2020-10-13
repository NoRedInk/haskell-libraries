{-# LANGUAGE GADTs #-}

module Redis.Mock
  ( handler,
  )
where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Database.Redis
import qualified List
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Tuple
import Prelude (IO, error, pure)

handler :: Text -> IO Internal.Handler
handler namespace = do
  hm <- newIORef HM.empty
  anything <- Platform.doAnythingHandler
  Internal.InternalHandler
    ( \query ->
        atomicModifyIORef' hm (doQuery query)
          |> Platform.doAnything anything
    )
    |> Internal.namespacedHandler namespace
    |> Prelude.pure

doQuery ::
  Internal.Query a ->
  HM.HashMap ByteString ByteString ->
  (HM.HashMap ByteString ByteString, Result Internal.Error a)
doQuery query hm =
  case query of
    Internal.Ping -> (hm, Ok Database.Redis.Pong)
    Internal.Get key -> (hm, Ok <| HM.lookup key hm)
    Internal.Set key value -> (HM.insert key value hm, Ok ())
    Internal.Getset key value -> (HM.insert key value hm, Ok <| HM.lookup key hm)
    Internal.Mget keys -> (hm, Ok <| List.map (\key -> HM.lookup key hm) keys)
    Internal.Mset assocs ->
      ( List.foldl
          (\(key, val) hm' -> HM.insert key val hm')
          hm
          assocs,
        Ok ()
      )
    Internal.Del keys ->
      List.foldl
        ( \key (hm', count) ->
            if HM.member key hm'
              then (HM.delete key hm', count + 1)
              else (hm', count)
        )
        (hm, 0 :: Int)
        keys
        |> Tuple.mapSecond Ok
    Internal.Hgetall _key -> error "No mock implementation implemented yet for hgetall"
    Internal.Hset _key _field _val -> error "No mock implementation implemented yet for hset"
    Internal.Hmset _key _vals -> error "No mock implementation implemented yet for hset"
    Internal.AtomicModify key f ->
      let (newValue, context) = HM.lookup key hm |> f
       in (HM.insert key newValue hm, Ok (newValue, context))
    Internal.Pure x -> (hm, Ok x)
    Internal.Apply fQuery xQuery ->
      let (hm1, f) = doQuery fQuery hm
          (hm2, x) = doQuery xQuery hm1
       in (hm2, map2 (\f' x' -> f' x') f x)
    Internal.WithResult f q ->
      doQuery q hm
        |> map
          ( \result ->
              case result of
                Err a -> Err a
                Ok res -> f res
          )
