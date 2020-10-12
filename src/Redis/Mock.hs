{-# LANGUAGE GADTs #-}

module Redis.Mock where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Database.Redis
import qualified List
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Task
import Prelude (IO, error, pure)

handler :: Text -> IO Internal.Handler
handler namespace = do
  hm <- newIORef HM.empty
  anything <- Platform.doAnythingHandler
  Internal.InternalHandler (doQuery hm anything)
    |> Internal.namespacedHandler namespace
    |> Prelude.pure

doQuery ::
  IORef (HM.HashMap ByteString ByteString) ->
  Platform.DoAnythingHandler ->
  Internal.Query a ->
  Task Internal.Error a
doQuery hm anything query =
  case query of
    Internal.Ping -> Task.succeed Database.Redis.Pong
    Internal.Get key ->
      Platform.doAnything
        anything
        (readIORef hm |> map (\hm' -> HM.lookup key hm' |> Ok))
    Internal.Set key value -> set hm anything key value
    Internal.Getset key value ->
      Platform.doAnything
        anything
        ( atomicModifyIORef'
            hm
            ( \hm' ->
                (HM.insert key value hm', HM.lookup key hm')
            )
            |> map Ok
        )
    Internal.Mget keys ->
      Platform.doAnything
        anything
        ( readIORef hm
            |> map
              ( \hm' ->
                  keys
                    |> List.map (\key -> HM.lookup key hm')
                    |> Ok
              )
        )
    Internal.Mset assocs ->
      assocs
        |> List.map (\(key, val) -> set hm anything key val)
        |> Task.sequence
        |> map (\_ -> ())
    Internal.Del keys ->
      Platform.doAnything
        anything
        ( atomicModifyIORef'
            hm
            ( \hm' ->
                List.foldl
                  ( \key (hm'', count) ->
                      if HM.member key hm''
                        then (HM.delete key hm'', count + 1)
                        else (hm'', count)
                  )
                  (hm', 0 :: Int)
                  keys
            )
            |> map Ok
        )
    Internal.Hgetall _key -> error "No mock implementation implemented yet for hgetall"
    Internal.Hset _key _field _val -> error "No mock implementation implemented yet for hset"
    Internal.AtomicModify key f ->
      Platform.doAnything
        anything
        ( atomicModifyIORef'
            hm
            ( \hm' ->
                let (newValue, context) = HM.lookup key hm' |> f
                 in (HM.insert key newValue hm', (newValue, context))
            )
            |> map Ok
        )
    Internal.Fmap f q -> doQuery hm anything q |> Task.map f

set ::
  IORef (HM.HashMap ByteString ByteString) ->
  Platform.DoAnythingHandler ->
  ByteString ->
  ByteString ->
  Task Internal.Error ()
set hm anything key value =
  Platform.doAnything
    anything
    (atomicModifyIORef' hm (\hm' -> (HM.insert key value hm', ())) |> map Ok)
