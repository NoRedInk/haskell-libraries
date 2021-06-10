{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Stub out Http requests in tests.
module Http.Mock
  ( stub,
  )
where

import qualified Data.Aeson
import qualified Data.IORef
import qualified Data.Text.Encoding
import qualified Debug
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Http.Internal as Internal
import qualified Platform
import qualified Task
import qualified Prelude

-- | Stub out http requests in a bit of code. You can use this if you don't
-- want your tests to make real http requests, and to listen in on the http
-- requests it is attempting to make.
--
-- 'stub' takes a function that it calls instead of making a real http request.
-- That function should return the response string and a optionally some
-- information about the http request. You'll get back the information collected
-- for each outgoing http request so you can run assertions against it.
--
-- > test "Stubbed HTTP requests" <| \_ -> do
-- >   urlsAccessed <-
-- >     Http.Mock.stub
-- >       (\req -> Task.succeed (Http.url req, "Response!"))
-- >       ( \http ->
-- >           Expect.succeeds <| do
-- >             _ <- Http.get http "example.com/one" Http.expectText
-- >             _ <- Http.get http "example.com/two" Http.expectText
-- >             Task.succeed ()
-- >       )
-- >   urlsAccessed
-- >     |> Expect.equal ["example.com/one", "example.com/two"]
stub ::
  Stack.HasCallStack =>
  (forall expect. Internal.Request expect -> Task Internal.Error (a, Text)) ->
  (Internal.Handler -> Expect.Expectation) ->
  Expect.Expectation' (List a)
stub respond stubbedTestBody = do
  logRef <- Expect.fromIO (Data.IORef.newIORef [])
  doAnything <- Expect.fromIO Platform.doAnythingHandler
  let mockHandler =
        Internal.Handler
          ( \req -> do
              (log, res) <- respond req
              Data.IORef.modifyIORef' logRef (\prev -> log : prev)
                |> map Ok
                |> Platform.doAnything doAnything
              case Internal.expect req of
                Internal.ExpectWhatever -> Task.succeed ()
                Internal.ExpectText -> Task.succeed res
                Internal.ExpectJson ->
                  case Data.Aeson.eitherDecodeStrict (Data.Text.Encoding.encodeUtf8 res) of
                    Prelude.Left err -> Task.fail (Internal.BadBody (Text.fromList err))
                    Prelude.Right decoded -> Task.succeed decoded
          )
          (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
          (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
  Expect.around (\f -> f mockHandler) (Stack.withFrozenCallStack stubbedTestBody)
  Expect.fromIO (Data.IORef.readIORef logRef)
    |> map List.reverse
