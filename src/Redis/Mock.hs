module Redis.Mock where

import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Database.Redis
import qualified List
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Task
import Prelude (IO, error, pure, uncurry)

handler :: Text -> IO Internal.Handler
handler namespace = do
  hm <- newIORef HM.empty
  anything <- Platform.doAnythingHandler
  let rawPing =
        pure Database.Redis.Pong
  let rawGet key =
        Platform.doAnything
          anything
          (readIORef hm |> andThen (\hm' -> HM.lookup key hm' |> Ok |> pure))
  let rawSet key value =
        Platform.doAnything
          anything
          (modifyIORef hm (HM.insert key value) |> map Ok)
  let rawGetSet key value =
        Platform.doAnything
          anything
          ( atomicModifyIORef
              hm
              ( \hm' ->
                  (HM.insert key value hm', HM.lookup key hm')
              )
              |> andThen (Ok >> pure)
          )
  let rawGetMany keys =
        Platform.doAnything
          anything
          ( readIORef hm
              |> andThen
                ( \hm' ->
                    keys
                      |> List.map (\key -> HM.lookup key hm')
                      |> Ok
                      |> pure
                )
          )
  let rawSetMany assocs =
        assocs
          |> List.map (uncurry rawSet)
          |> Task.sequence
          |> map (\_ -> ())
  let del keys =
        Platform.doAnything
          anything
          ( atomicModifyIORef
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
  let atomicModify key f =
        Platform.doAnything
          anything
          ( atomicModifyIORef
              hm
              ( \hm' ->
                  let (newValue, context) = HM.lookup key hm' |> f
                   in (HM.insert key newValue hm', (newValue, context))
              )
              |> map Ok
          )
  pure
    <| ( Internal.InternalHandler
           { Internal.rawPing = rawPing,
             Internal.rawGet = rawGet,
             Internal.rawSet = rawSet,
             Internal.rawGetSet = rawGetSet,
             Internal.rawGetMany = rawGetMany,
             Internal.rawSetMany = rawSetMany,
             Internal.rawDelete = del,
             Internal.rawHGetAll = \_ -> error "No mock implementation implemented yet for hgetall",
             Internal.rawHSet = \_ -> error "No mock implementation implemented yet for hset",
             Internal.rawHMSet = \_ -> error "No mock implementation implemented yet for hset",
             Internal.rawAtomicModify = atomicModify
           }
           |> Internal.namespacedHandler namespace
       )
