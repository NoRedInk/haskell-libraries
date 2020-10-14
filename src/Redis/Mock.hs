{-# LANGUAGE GADTs #-}

module Redis.Mock
  ( handler,
  )
where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Database.Redis
import qualified List
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Tuple
import Prelude (IO, error, pure)

handler :: Text -> IO Internal.Handler
handler namespace = do
  model <- init
  anything <- Platform.doAnythingHandler
  Internal.InternalHandler
    { Internal.doQuery = \query ->
        atomicModifyIORef' model (doQuery query)
          |> Platform.doAnything anything,
      Internal.doTransaction = \query ->
        atomicModifyIORef' model (doQuery query)
          |> Platform.doAnything anything,
      Internal.watch = \_ -> pure () -- TODO: implement this
    }
    |> Internal.namespacedHandler namespace
    |> Prelude.pure

-- | This is our mock implementation of the Redis state. Our mock implementation
-- will store a single value of this type, and redis commands will modify it.
newtype Model = Model {hash :: HM.HashMap ByteString ByteString}

withHash :: (HM.HashMap ByteString ByteString -> HM.HashMap ByteString ByteString) -> Model -> Model
withHash f (Model hm) = Model (f hm)

init :: IO (IORef Model)
init = newIORef (Model HM.empty)

doQuery ::
  Internal.Query a ->
  Model ->
  (Model, Result Internal.Error a)
doQuery query model =
  case query of
    Internal.Ping ->
      ( model,
        Ok Database.Redis.Pong
      )
    Internal.Get key ->
      ( model,
        Ok <| HM.lookup key (hash model)
      )
    Internal.Set key value ->
      ( withHash (HM.insert key value) model,
        Ok ()
      )
    Internal.Getset key value ->
      ( withHash (HM.insert key value) model,
        Ok <| HM.lookup key (hash model)
      )
    Internal.Mget keys ->
      ( model,
        Ok <| List.map (\key -> HM.lookup key (hash model)) keys
      )
    Internal.Mset assocs ->
      ( List.foldl
          (\(key, val) model' -> withHash (HM.insert key val) model')
          model
          assocs,
        Ok ()
      )
    Internal.Del keys ->
      List.foldl
        ( \key (model', count) ->
            if HM.member key (hash model')
              then (withHash (HM.delete key) model', count + 1)
              else (model', count)
        )
        (model, 0 :: Int)
        keys
        |> Tuple.mapSecond Ok
    Internal.Hgetall _key -> error "No mock implementation implemented yet for hgetall"
    Internal.Hset _key _field _val -> error "No mock implementation implemented yet for hset"
    Internal.Hmset _key _vals -> error "No mock implementation implemented yet for hset"
    Internal.Pure x -> (model, Ok x)
    Internal.Apply fQuery xQuery ->
      let (hm1, f) = doQuery fQuery model
          (hm2, x) = doQuery xQuery hm1
       in (hm2, map2 (\f' x' -> f' x') f x)
    Internal.WithResult f q ->
      doQuery q model
        |> map
          ( \result ->
              case result of
                Err a -> Err a
                Ok res -> f res
          )
