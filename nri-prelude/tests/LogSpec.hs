module LogSpec (tests) where

import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Data.IORef as IORef
import qualified Debug
import qualified Expect
import qualified GHC.Stack as Stack
import Log
import NriPrelude
import qualified Platform.Internal as Internal
import qualified Task
import Test (Test, describe, test)
import qualified Text
import qualified Text.Show.Pretty
import qualified Prelude

-- | These tests would likely benefit from golden test support, so we don't
-- need to store these expectation strings inside of the files here and can
-- update them more easily if they change.
tests :: Test
tests =
  describe
    "Log"
    [ test "`info` produces expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <- info "logging a message!" [context "a number" (12 :: Int)] |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-info",
      test "`userIsAnnoyed` produces expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              userIsAnnoyed
                "the button didn't work"
                "fix the button"
                [context "button" ("PRESS" :: Text)]
                |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-user-is-annoyed",
      test "`userIsPained` produces expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              userIsPained
                "user cut themselves on the modal"
                "file modal's sharp edges"
                [context "modal" ("SURPRISE!" :: Text)]
                |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-user-is-pained",
      test "`userIsBlocked` produces expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              userIsBlocked
                "door is blocked"
                "find key"
                [context "house number" (5 :: Int)]
                |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-user-is-blocked",
      test "nested spans pruduce expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              info "log!" []
                |> withContext "inner span" [context "word" ("sabbatical" :: Text)]
                |> withContext "outer span" [context "number" (825 :: Int)]
                |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-nested-spans",
      test "unexpected exceptions produce expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              Internal.Task (\_ -> Exception.throwIO TestException)
                |> withContext "inner span" [context "word" ("sabbatical" :: Text)]
                |> withContext "outer span" [context "number" (825 :: Int)]
                |> Task.attempt handler
                |> Exception.handle (\TestException -> Prelude.pure (Ok ()))
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-unexpected-exceptions",
      test "async exceptions produce expected debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            threadId <- Control.Concurrent.myThreadId
            _ <-
              Internal.Task
                ( \_ -> do
                    Exception.throwTo threadId TestException
                    Prelude.pure (Ok ())
                )
                |> withContext "inner span" [context "word" ("sabbatical" :: Text)]
                |> withContext "outer span" [context "number" (825 :: Int)]
                |> Task.attempt handler
                |> Exception.handleAsync (\(Exception.AsyncExceptionWrapper _) -> Prelude.pure (Ok ()))
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Expect.equalToContentsOf "tests/golden-results/log-async-exceptions",
      test "secrets do not appear in debugging info" <| \_ -> do
        spans <-
          Expect.fromIO <| do
            (recordedTracingSpans, handler) <- newHandler
            _ <-
              info
                "logging a message!"
                [context "secret" (Log.mkSecret ("Mango's are delicious" :: Text))]
                |> Task.attempt handler
            recordedTracingSpans
        spans
          |> Debug.toString
          |> Text.contains "Mango"
          |> Expect.equal False,
      -- Haskell's default @show@ instance prints the shown Haskell value on a
      -- single line. This isn't great when using @show@ to debug larger Haskell
      -- values. That's why our @Debug.toString@ implementation uses the
      -- @pretty-show@ package for rendering a Haskell value as a string, which
      -- provides the alternative @ppShow@ function for printing Haskell values
      -- with newlines. @ppShow@ works for all Haskell types with a @Show@
      -- instance.
      --
      -- @ppShow@ works internally by using regular @show@, parsing the
      -- generated string into some type representing an arbitrary Haskell
      -- value, then printing that again in a nicer way. The library relies on
      -- Haskell's derived @Show@ instances looking a particular way. When we
      -- define a custom @Show@ instance for a type we're not bound to the
      -- conventions Haskell uses for it's automatically generated @Show@
      -- instances, and @ppShow@ might not know how to parse the string we
      -- produce for a value. Should @ppShow@ fail to use its parser it falls
      -- back to using regular @show@ for presenting the value, so without
      -- newlines.
      --
      -- Generally Haskell recommends you do not write your own @Show@
      -- instances. We provide one for @Secret@ to help us accidentally put
      -- sensitive values in our logs. This test checks our custom @Secret@
      -- show instance can be parsed by @ppShow@, ensuring that @Secret@ values
      -- and larger types containing @Secret@ values can be pretty printed by
      -- @Debug.toString@.
      test "`Debug.toString` can pretty-print values containing secrets" <| \_ ->
        Log.mkSecret ()
          |> Text.Show.Pretty.reify
          |> Expect.notEqual Nothing
    ]

data TestException = TestException deriving (Show)

instance Exception.Exception TestException

newHandler :: Stack.HasCallStack => Prelude.IO (Prelude.IO [Internal.TracingSpan], Internal.LogHandler)
newHandler = do
  recordedTracingSpans <- IORef.newIORef []
  handler <-
    Stack.withFrozenCallStack
      Internal.mkHandler
      ""
      (Internal.Clock (Prelude.pure 0))
      (IORef.writeIORef recordedTracingSpans << Internal.children)
      ""
  Prelude.pure
    ( do
        Internal.finishTracingSpan handler Nothing
        IORef.readIORef recordedTracingSpans
          |> map (map (recurse (\span -> span {Internal.allocated = 0}))),
      handler
    )

recurse :: (Internal.TracingSpan -> Internal.TracingSpan) -> Internal.TracingSpan -> Internal.TracingSpan
recurse f span =
  (f span)
    { Internal.children = map (recurse f) (Internal.children span)
    }
