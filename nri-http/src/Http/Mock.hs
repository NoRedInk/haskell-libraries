{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Stub out Http requests in tests.
module Http.Mock
  ( stub,
    Stub,
    mkStub,

    -- * Read request data
    getHeader,
    getTextBody,
    getJsonBody,
    getBytesBody,
  )
where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Dynamic as Dynamic
import qualified Data.IORef
import Data.String (fromString)
import qualified Data.Text.Encoding
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
    (Dynamic.Typeable e, Dynamic.Typeable expect) =>
    (Internal.Request' e expect -> Task e (a, expect)) ->
    Stub a

-- | Create a 'Stub'.
mkStub ::
  (Dynamic.Typeable e, Dynamic.Typeable expect) =>
  (Internal.Request' e expect -> Task e (a, expect)) ->
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
              (log, res) <- tryRespond responders req
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

-- | Read the body of the request as text. Useful to check what data got
-- submitted inside a 'stub' function.
--
-- This will return 'Nothing' if the body cannot be parsed as UTF8 text.
getTextBody :: Internal.Request expect -> Maybe Text
getTextBody req =
  Data.Text.Encoding.decodeUtf8' (getBytesBody req)
    |> eitherToMaybe

-- | Read the body of the request as json. Useful to check what data got
-- submitted inside a 'stub' function.
--
-- This will return an error if parsing the JSON body fails.
getJsonBody :: Aeson.FromJSON a => Internal.Request expect -> Result Text a
getJsonBody req =
  case Aeson.eitherDecodeStrict (getBytesBody req) of
    Prelude.Left err -> Err (Text.fromList err)
    Prelude.Right decoded -> Ok decoded

-- | Read the body of the request as bytes. Useful to check what data got
-- submitted inside a 'stub' function.
getBytesBody :: Internal.Request expect -> ByteString
getBytesBody req =
  Internal.body req
    |> Internal.bodyContents
    |> Data.ByteString.Lazy.toStrict

-- | Read a header of the request. Useful to check what data got submitted
-- inside a 'stub' function.
--
-- This will return 'Nothing' if no header with that name was set on the
-- request.
getHeader :: Text -> Internal.Request expect -> Maybe Text
getHeader name req =
  Internal.headers req
    |> List.map Internal.unHeader
    |> Prelude.lookup (fromString (Text.toList name))
    |> Maybe.andThen (eitherToMaybe << Data.Text.Encoding.decodeUtf8')

eitherToMaybe :: Prelude.Either a b -> Maybe b
eitherToMaybe either =
  case either of
    Prelude.Left _ -> Nothing
    Prelude.Right x -> Just x

tryRespond ::
  ( Dynamic.Typeable expect,
    Dynamic.Typeable e,
    Dynamic.Typeable a
  ) =>
  List (Stub a) ->
  Internal.Request' e expect ->
  Task e (a, expect)
tryRespond [] req =
  let msg =
        "Http request was made with expected return type "
          ++ printType req
          ++ ", but I don't how to create a mock response of this type. Please add a `mkStub` entry for this type in the test."
      handleCustomResponse :: (Internal.Response s -> Result e expect) -> Task e (a, expect)
      handleCustomResponse f = case f (Internal.NetworkError_ msg) of
        Err err -> Task.fail err
        Ok _ -> Debug.todo "Since we manually craft the Response as an Error, this case will not run."
   in case Internal.expect req of
        Internal.ExpectJson ->
          Task.fail (Internal.NetworkError msg)
        Internal.ExpectText ->
          Task.fail (Internal.NetworkError msg)
        Internal.ExpectWhatever ->
          Task.fail (Internal.NetworkError msg)
        Internal.ExpectTextResponse f ->
          handleCustomResponse f
        Internal.ExpectBytesResponse f ->
          handleCustomResponse f
tryRespond (Stub respond : rest) req =
  Dynamic.dynApply (Dynamic.toDyn respond) (Dynamic.toDyn req)
    |> Maybe.andThen Dynamic.fromDynamic
    |> Maybe.withDefault (tryRespond rest req)

printType :: Dynamic.Typeable expect => proxy expect -> Text
printType expect =
  Type.Reflection.someTypeRep expect
    |> Debug.toString
