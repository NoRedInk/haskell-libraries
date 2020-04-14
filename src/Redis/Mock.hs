module Redis.Mock where

import Cherry.Prelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Acquire
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified List
import qualified Platform
import qualified Redis.Internal as Internal
import Prelude (pure)

handler :: Data.Acquire.Acquire Internal.Handler
handler = do
  hm <- liftIO <| newIORef HM.empty
  anything <- liftIO Platform.doAnythingHandler
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
  let delete keys =
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
  pure Internal.Handler
    { Internal.rawGet = rawGet,
      Internal.rawSet = rawSet,
      Internal.rawGetSet = rawGetSet,
      Internal.rawDelete = delete
    }
