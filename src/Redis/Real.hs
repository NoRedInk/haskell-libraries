module Redis.Real where

import Cherry.Prelude
import qualified Control.Monad.Catch
import qualified Data.Acquire
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.Redis
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Settings as Settings
import qualified Task
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
             Internal.rawGetMany = rawGetMany connection anything,
             Internal.rawSetMany = rawSetMany connection anything,
             Internal.rawDelete = rawDelete connection anything,
             Internal.rawAtomicModify = rawAtomicModify connection anything
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
        |> map toResult
        |> (\r -> Control.Monad.Catch.catch r (\(_ :: Database.Redis.ConnectionLostException) -> pure <| Err Internal.ConnectionLost))
    )

toResult :: Either Database.Redis.Reply a -> Result Internal.Error a
toResult reply =
  case reply of
    Left (Database.Redis.Error err) -> Err (Internal.RedisError <| Data.Text.Encoding.decodeUtf8 err)
    Left _ -> Err (Internal.RedisError "The Redis library said this was an error but returned no error message.")
    Right r -> Ok r

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

rawGetMany ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  [Data.ByteString.ByteString] ->
  Task Internal.Error [Maybe Data.ByteString.ByteString]
rawGetMany connection anything keys =
  platformRedis connection anything (Database.Redis.mget keys)

rawSetMany ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  [(Data.ByteString.ByteString, Data.ByteString.ByteString)] ->
  Task Internal.Error ()
rawSetMany connection anything assocs =
  platformRedis connection anything (Database.Redis.mset assocs)
    |> map (\_ -> ())

rawDelete ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  [Data.ByteString.ByteString] ->
  Task Internal.Error Int
rawDelete connection anything keys =
  platformRedis connection anything (Database.Redis.del keys)
    |> map fromIntegral

rawAtomicModify ::
  Database.Redis.Connection ->
  Platform.DoAnythingHandler ->
  Data.ByteString.ByteString ->
  (Maybe Data.ByteString.ByteString -> (Data.ByteString.ByteString, a)) ->
  Task Internal.Error (Data.ByteString.ByteString, a)
rawAtomicModify connection anything key f =
  inner (100 :: Int)
  where
    inner count =
      platformRedis
        connection
        anything
        action
        |> andThen (processTxResult count)
    processTxResult count (txResult, newValue) =
      case txResult of
        Database.Redis.TxSuccess _ -> pure newValue
        Database.Redis.TxAborted ->
          if count > 0
            then inner (count - 1)
            else Task.fail <| Internal.RedisError "Attempted atomic update 100 times without success."
        Database.Redis.TxError err -> Task.fail <| Internal.RedisError (Data.Text.pack err)
    action = do
      _ <- Database.Redis.watch [key]
      resp <- Database.Redis.get key
      case resp of
        Right r -> do
          let (newValue, context) = f r
          txResult <- Database.Redis.multiExec (Database.Redis.set key newValue)
          pure <| Right (txResult, (newValue, context))
        Left e' -> pure <| Left e'
