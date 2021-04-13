{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

-- | This module is dedicated to logging information in production, to help
-- understand what the application is doing when something goes wrong. This sets
-- it apart from the @Debug@ module which provide helpers for debugging problems
-- in development.
--
-- This module does not have an Elm counterpart.
module Log
  ( -- * Logging
    debug,
    info,
    warn,
    error,
    withContext,
    context,

    -- * Secrets
    Secret,
    mkSecret,
    unSecret,

    -- * For use in observability modules
    Context (..),
    LogContexts (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform
import qualified Platform.Internal as Internal
import qualified Task
import qualified Text.Show
import qualified Prelude

-- | A log message that is probably only useful in development, or when we're
-- really confused about something and need ALL THE CONTEXT.
--
-- In addition to a log message you can pass additional key-value pairs with
-- information that might be relevant for debugging.
--
-- > debug "Computation partially succeeded" [context "answer" 2]
debug :: Stack.HasCallStack => Text -> [Context] -> Task e ()
debug message contexts =
  Stack.withFrozenCallStack
    log
    message
    ReportAsSucceeded
    (Context "level" Debug : contexts)

-- | A log message useful for when things have gone off the rails.
-- We should have a ton of messages at this level.
-- It should help us out when we're dealing with something hard.
--
-- In addition to a log message you can pass additional key-value pairs with
-- information that might be relevant for debugging.
--
-- > info "I added 1 and 1" [context "answer" 2]
info :: Stack.HasCallStack => Text -> [Context] -> Task e ()
info message contexts =
  Stack.withFrozenCallStack
    log
    message
    ReportAsSucceeded
    (Context "level" Info : contexts)

-- | A log message when something went wrong, but it did not go wrong in a way
-- to totally break the thing we're doing. These should be triaged and fixed
-- soon, but aren't show-stoppers.
--
-- In addition to a log message you can pass additional key-value pairs with
-- information that might be relevant for debugging.
--
-- > warn "This field was sent, but we're gonna deprecate it!" []
warn :: Stack.HasCallStack => Text -> [Context] -> Task e ()
warn message contexts =
  Stack.withFrozenCallStack
    log
    message
    ReportAsFailed
    (Context "level" Warn : contexts)

-- | A log message when we can't continue with what we were trying to do
-- because of a problem.
--
-- In addition to a log message you can pass additional key-value pairs with
-- information that might be relevant for debugging.
--
-- > error "The user tried to request this thing, but they aren't allowed!" []
error :: Stack.HasCallStack => Text -> [Context] -> Task e ()
error message contexts =
  Stack.withFrozenCallStack
    log
    message
    ReportAsFailed
    (Context "level" Error : contexts)

-- | Mark a block of code as a logical unit by giving it a name. This name will
-- be used in logs and monitoring dashboards, so use this function to help
-- debug production problems.
--
-- In addition to a name you can pass this function a list of context. A
-- context is a key-value pair you want to attach to all logs made inside of
-- the block of code wrapped.
--
-- Example usage:
--
-- > withContext "play-music" [context "artist" "The Beatles"] <| do
-- >   -- your code here!
--
-- Additionally, this function adds an entry to our homemade stack trace for if something errors.
-- Why not use the built-in stack trace? Well, the built-in stack trace only records a frame if you
-- add @Stack.HasCallStack =>@ to the function, so if we want a full stack trace, we need to add
-- that to literally all functions. Instead of doing that, we will use @withContext@ to collect
-- the stack trace, since it is used fairly often already. It will not be complete either, but
-- it's the best we can do without too much trouble.
withContext ::
  Stack.HasCallStack =>
  Text ->
  [Context] ->
  Task e b ->
  Task e b
withContext name contexts task =
  Stack.withFrozenCallStack
    Internal.tracingSpan
    name
    ( Platform.finally
        task
        ( do
            Platform.setTracingSpanDetails (LogContexts contexts)
            Platform.setTracingSpanSummary name
        )
    )

--
-- CONTEXT
--

-- | A key-value pair that can be added to a log context. All log expressions
-- within the context will always log this key-value pair.
context :: (Aeson.ToJSON a) => Text -> a -> Context
context = Context

-- | Extra information to attach to a log message. It is passed a string key
-- defining what the data is and a value with a @ToJSON@ instance.
data Context where
  Context :: Aeson.ToJSON a => Text -> a -> Context

instance Show Context where
  show (Context key value) =
    Aeson.encode (key, value)
      |> Prelude.show

-- | A set of log contexts.
newtype LogContexts
  = LogContexts [Context]

instance Aeson.ToJSON LogContexts where
  toJSON (LogContexts contexts) =
    contexts
      |> map (\(Context key val) -> key .= val)
      |> Aeson.object

  toEncoding (LogContexts contexts) =
    contexts
      |> Prelude.foldMap (\(Context key val) -> key .= val)
      |> Aeson.pairs

instance Internal.TracingSpanDetails LogContexts

--
-- SECRET
--

-- | Wrap a value in a secret to prevent it from being accidentally logged.
--
-- > Debug.log "Logging a secret" (mkSecret "My PIN is 1234")
-- > --> Logging a secret: Secret *****
mkSecret :: a -> Secret a
mkSecret = Secret

-- | Retrieve the original value from a secret. Be very careful with this and ask
-- yourself: is there really no way I can pass this value on as a secret
-- further before I need to unwrap it?
--
-- The longer a value is wrapped in a Secret, the smaller the odds of it
-- accidentally being logged.
unSecret :: Secret a -> a
unSecret (Secret x) = x

-- | Distinguishes data that is secret and should not be logged.
--
-- Please be careful when defining or altering instances for this data type.
-- There's a good chance we will leak credentials, PII, or
-- other equally sensitive information.
newtype Secret a
  = Secret a
  deriving (Prelude.Eq, Prelude.Functor)

instance Prelude.Applicative Secret where
  Secret f <*> Secret x = Secret (f x)

  pure = Secret

-- | N.B. This instance of 'Show' is not law abiding.
--
-- This instance exists because we sometimes use 'Secret' in data types
-- that have to derive 'Show' (due to other constraints on those data types).
--
-- This is not a pattern to follow; it's an exception.
instance Show (Secret a) where
  showsPrec p _ =
    Text.Show.showParen (p > 10) (Text.Show.showString "Secret \"*****\"")

instance Aeson.ToJSON (Secret a) where
  toJSON _ = Aeson.String "Secret *****"

--
-- TRIAGE
--

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Generic)

instance Aeson.ToJSON LogLevel

-- ReportAsFailed marks the request as a failure in logging, but has no impact on the resulting Task. E.g. will not trigger a 500 error but will report an error to, e.g. BugSnag.
data ReportStatus = ReportAsFailed | ReportAsSucceeded

log :: Stack.HasCallStack => Text -> ReportStatus -> [Context] -> Task e ()
log msg reportStatus contexts =
  Internal.tracingSpan msg <| do
    Platform.setTracingSpanDetails (LogContexts contexts)
    case reportStatus of
      ReportAsSucceeded -> Task.succeed ()
      ReportAsFailed -> Platform.markTracingSpanFailed
