{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Http.Mock where

import Cherry.Prelude
import Data.IORef
import Data.Typeable
import qualified Debug
import Internal.Http
import qualified Platform
import qualified Task
import Prelude (IO, pure)

testHandler :: (forall expect. Settings expect -> Task Error expect) -> IO Handler
testHandler mockServer = do
  pure
    <| Internal.Http.Handler
      mockServer
      (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
      (\_ -> Debug.todo "We don't mock third party HTTP calls yet")

simpleMockServer :: Typeable mock => Platform.DoAnythingHandler -> IORef (Maybe (Settings mock)) -> mock -> Settings expect -> Task Error expect
simpleMockServer doAnything ioRef v s =
  case Internal.Http._expect s of
    Internal.Http.ExpectWhatever ->
      case (cast s) of
        Just settings -> do
          Platform.doAnything
            doAnything
            (map Ok (writeIORef ioRef (Just settings)))
          Task.succeed ()
        _ -> Task.fail (Internal.Http.NetworkError "You expected a response of the wrong type in your mock HTTP request.")
    Internal.Http.ExpectText ->
      case (cast v, cast s) of
        (Just response, Just settings) -> do
          Platform.doAnything
            doAnything
            (map Ok (writeIORef ioRef (Just settings)))
          Task.succeed
            response
        _ -> Task.fail (Internal.Http.NetworkError "You expected a response of the wrong type in your mock HTTP request.")
    Internal.Http.ExpectJson ->
      case (cast v, cast s) of
        (Just response, Just settings) -> do
          Platform.doAnything
            doAnything
            (map Ok (writeIORef ioRef (Just settings)))
          Task.succeed
            response
        _ -> Task.fail (Internal.Http.NetworkError "You expected a response of the wrong type in your mock HTTP request.")

simpleTestHandler :: Typeable result => result -> IO (Handler, IORef (Maybe (Settings result)))
simpleTestHandler responseValue = do
  doAnything <- Platform.doAnythingHandler
  ioRef <- newIORef Nothing
  h <- testHandler (simpleMockServer doAnything ioRef responseValue)
  pure (h, ioRef)
