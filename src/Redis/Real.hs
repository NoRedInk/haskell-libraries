module Redis.Real where

import Cherry.Prelude
import qualified Control.Monad.Catch
import qualified Data.Acquire
import qualified Data.ByteString
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import Prelude (Either (Left, Right), IO, fromIntegral, fst, pure)

handler :: Settings.Settings -> Data.Acquire.Acquire Internal.Handler
handler settings =
  Data.Acquire.mkAcquire (acquireHandler settings) releaseHandler
    |> map fst

acquireHandler :: Settings.Settings -> IO (Internal.Handler, Database.Redis.Connection)
acquireHandler settings = do
  connection <- Database.Redis.checkedConnect (Settings.connectionInfo settings)
  anything <- Platform.doAnythingHandler
  pure
    <| ( Internal.Handler
           { Internal.rawGet = rawGet connection anything,
             Internal.rawSet = rawSet connection anything,
             Internal.rawGetSet = rawGetSet connection anything,
             Internal.rawDelete = rawDelete connection anything
           },
         connection
       )

releaseHandler :: (Internal.Handler, Database.Redis.Connection) -> IO ()
releaseHandler (_, connection) = Database.Redis.disconnect connection

platformRedis ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  Database.Redis.Redis (Either Database.Redis.Reply a) ->
  Task Internal.Error a
platformRedis connection anything action =
  Platform.doAnything
    anything
    ( Database.Redis.runRedis connection action
        |> map
          ( \rep -> case rep of
              Left (Database.Redis.Error err) -> Err (Internal.RedisError <| Data.Text.Encoding.decodeUtf8 err)
              Left _ -> Err (Internal.RedisError "The Redis library said this was an error but returned no error message.")
              Right r -> Ok r
          )
        |> (\r -> Control.Monad.Catch.catch r (\(_ :: Database.Redis.ConnectionLostException) -> pure <| Err Internal.ConnectionLost))
    )

rawGet ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Task Internal.Error (Maybe Data.ByteString.ByteString)
rawGet connection anything key =
  platformRedis connection anything (Database.Redis.get key)

rawSet ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Task Internal.Error ()
rawSet connection anything key value =
  platformRedis connection anything (Database.Redis.set key value)
    |> map (\_ -> ())

rawGetSet ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  Task Internal.Error (Maybe Data.ByteString.ByteString)
rawGetSet connection anything key value =
  platformRedis connection anything (Database.Redis.getset key value)

rawDelete ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  [Data.ByteString.ByteString] ->
  Task Internal.Error Int
rawDelete connection anything keys =
  platformRedis connection anything (Database.Redis.del keys)
    |> map fromIntegral
