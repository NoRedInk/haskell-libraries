{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Stub out Http requests in tests.
module Http.Mock
  ( stub,
    Stub,
    mkStub,
  )
where

import qualified Data.Dynamic as Dynamic
import qualified Data.IORef
import qualified Debug
import qualified Expect
import qualified GHC.Stack as Stack
import qualified Http.Internal as Internal
import qualified Platform
import qualified Task
import qualified Type.Reflection
import qualified Prelude

-- | A stub for a single request type. If your test body can perform multiple
-- different kinds of http requests, you'll want one of these per request type.
data Stub a where
  Stub ::
    Dynamic.Typeable expect =>
    (Internal.Request expect -> Task Internal.Error (a, expect)) ->
    Stub a

-- | Create a 'Stub'.
mkStub ::
  Dynamic.Typeable expect =>
  (Internal.Request expect -> Task Internal.Error (a, expect)) ->
  Stub a
mkStub = Stub

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
-- >       [mkStub (\req -> Task.succeed (Http.url req, "Response!" :: Text))]
-- >       ( \http ->
-- >           Expect.succeeds <| do
-- >             _ <- Http.get http "example.com/one" Http.expectText
-- >             _ <- Http.get http "example.com/two" Http.expectText
-- >             Task.succeed ()
-- >       )
-- >   urlsAccessed
-- >     |> Expect.equal ["example.com/one", "example.com/two"]
stub ::
  ( Stack.HasCallStack,
    Dynamic.Typeable a
  ) =>
  (List (Stub a)) ->
  (Internal.Handler -> Expect.Expectation) ->
  Expect.Expectation' (List a)
stub responders stubbedTestBody = do
  logRef <- Expect.fromIO (Data.IORef.newIORef [])
  doAnything <- Expect.fromIO Platform.doAnythingHandler
  let mockHandler =
        Internal.Handler
          ( \req -> do
              (log, res) <-
                tryRespond
                  responders
                  ( Internal.NetworkError
                      ( "Http request was made with expected return type "
                          ++ printType req
                          ++ ", but I don't how to create a mock response of this type. Please add a `mkStub` entry for this type in the test."
                      )
                  )
                  req
              Data.IORef.modifyIORef' logRef (\prev -> log : prev)
                |> map Ok
                |> Platform.doAnything doAnything
              Prelude.pure res
          )
          (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
          (\_ -> Debug.todo "We don't mock third party HTTP calls yet")
  Expect.around (\f -> f mockHandler) (Stack.withFrozenCallStack stubbedTestBody)
  Expect.fromIO (Data.IORef.readIORef logRef)
    |> map List.reverse

tryRespond ::
  ( Dynamic.Typeable expect,
    Dynamic.Typeable a
  ) =>
  List (Stub a) ->
  Internal.Error ->
  Internal.Request expect ->
  Task Internal.Error (a, expect)
tryRespond [] err _ = Task.fail err
tryRespond (Stub respond : rest) err req =
  Dynamic.dynApply (Dynamic.toDyn respond) (Dynamic.toDyn req)
    |> Maybe.andThen Dynamic.fromDynamic
    |> Maybe.withDefault (tryRespond rest err req)

printType :: Dynamic.Typeable expect => proxy expect -> Text
printType expect =
  Type.Reflection.someTypeRep expect
    |> Debug.toString
