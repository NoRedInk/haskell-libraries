{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Http.Mock
  ( simpleJsonTestHandler,
    simpleWhateverTestHandler,
    testHandler,
  )
where

import Cherry.Prelude
import qualified Data.Aeson as Aeson
import Data.IORef
import qualified Debug
import Internal.Http
import qualified Platform
import qualified Task
import Prelude (Either (Left, Right), IO, pure)

testHandler :: (forall expect. Settings expect -> Task Error expect) -> IO Handler
testHandler mockServer =
  pure
    <| Internal.Http.Handler
      mockServer
      (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
      (\_ -> Debug.todo "We don't mock third party HTTP calls yet")

simpleJsonMockServer :: Aeson.ToJSON mock => Platform.DoAnythingHandler -> IORef (Maybe (Settings ())) -> mock -> Settings expect -> Task Error expect
simpleJsonMockServer doAnything ioRef responseValue capturedRequestSettings =
  case Internal.Http._expect capturedRequestSettings of
    Internal.Http.ExpectWhatever ->
      Task.fail (Internal.Http.NetworkError "You said you expected a JSON request, but 'ExpectWhatever' was sent")
    Internal.Http.ExpectText ->
      Task.fail (Internal.Http.NetworkError "You said you expected a JSON request, but 'ExpectText' was sent")
    Internal.Http.ExpectJson ->
      case responseValue |> Aeson.encode |> Aeson.eitherDecode of
        Left _ ->
          Task.fail (Internal.Http.NetworkError "We couldn't transform the mock JSON you provided into a valid response.")
        Right response -> do
          Platform.doAnything
            doAnything
            (map Ok (writeIORef ioRef (Just capturedRequestSettings {_expect = Internal.Http.ExpectWhatever})))
          Task.succeed
            response

simpleJsonTestHandler :: Aeson.ToJSON result => result -> IO (Handler, IORef (Maybe (Settings ())))
simpleJsonTestHandler responseValue = do
  doAnything <- Platform.doAnythingHandler
  ioRef <- newIORef Nothing
  h <- testHandler (simpleJsonMockServer doAnything ioRef responseValue)
  pure (h, ioRef)

simpleWhateverMockServer :: Platform.DoAnythingHandler -> IORef (Maybe (Settings ())) -> Settings expect -> Task Error expect
simpleWhateverMockServer doAnything ioRef capturedRequestSettings =
  case Internal.Http._expect capturedRequestSettings of
    Internal.Http.ExpectWhatever -> do
      Platform.doAnything
        doAnything
        (map Ok (writeIORef ioRef (Just capturedRequestSettings)))
      Task.succeed ()
    Internal.Http.ExpectText ->
      Task.fail (Internal.Http.NetworkError "You said you didn't care about the response, but 'ExpectText' was sent")
    Internal.Http.ExpectJson ->
      Task.fail (Internal.Http.NetworkError "You said you didn't care about the response, but 'ExpectJson' was sent")

simpleWhateverTestHandler :: IO (Handler, IORef (Maybe (Settings ())))
simpleWhateverTestHandler = do
  doAnything <- Platform.doAnythingHandler
  ioRef <- newIORef Nothing
  h <- testHandler (simpleWhateverMockServer doAnything ioRef)
  pure (h, ioRef)
