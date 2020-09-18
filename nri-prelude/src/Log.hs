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
    info,
    userIsAnnoyed,
    userIsConfused,
    userIsPained,
    userIsBlocked,
    withContext,
    context,

    -- * Secrets
    Secret,
    mkSecret,
    unSecret,

    -- * For use in observability modules
    Context (..),
    LogContexts (..),
    TriageInfo (..),
    Impact (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified GHC.Stack as Stack
import NriPrelude
import qualified Platform
import qualified Platform.Internal as Internal
import qualified Task
import qualified Text.Show
import qualified Prelude

-- | A log message useful for when things have gone off the rails.
-- We should have a ton of messages at this level.
-- It should help us out when we're dealing with something hard.
--
-- In addition to a log message you can pass additional key-value pairs with
-- information that might be relevant for debugging.
--
--     info "I added 1 and 1" [context "answer" 2]
info :: Stack.HasCallStack => Text -> [Context] -> Task e ()
info message contexts = Stack.withFrozenCallStack log message True contexts

-- | A log message when the user is annoyed, but not blocked.
--
--   Log.userIsAnnoyed
--     "We poked the user unnecessarily."
--     "Try to stop poking the user."
--     [ Log.context "The type of poking stick" poker ]
userIsAnnoyed :: Stack.HasCallStack => Text -> Text -> [Context] -> Task e ()
userIsAnnoyed message advisory contexts =
  let triage = TriageInfo UserAnnoyed advisory
   in Stack.withFrozenCallStack
        log
        message
        False
        (Context "triage" triage : contexts)

-- | Like @userIsAnnoyed@, but when the user is userIsConfused.
userIsConfused :: Stack.HasCallStack => Text -> Text -> [Context] -> Task e ()
userIsConfused message advisory contexts =
  let triage = TriageInfo UserConfused advisory
   in Stack.withFrozenCallStack
        log
        message
        False
        (Context "triage" triage : contexts)

-- | Like @userIsAnnoyed@, but when the user is in pain.
userIsPained :: Stack.HasCallStack => Text -> Text -> [Context] -> Task e ()
userIsPained message advisory contexts =
  let triage = TriageInfo UserInPain advisory
   in Stack.withFrozenCallStack
        log
        message
        False
        (Context "triage" triage : contexts)

-- | Like @userIsAnnoyed@, but when the user is blocked.
userIsBlocked :: Stack.HasCallStack => Text -> Text -> [Context] -> Task e ()
userIsBlocked message advisory contexts =
  let triage = TriageInfo UserBlocked advisory
   in Stack.withFrozenCallStack
        log
        message
        False
        (Context "triage" triage : contexts)

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
--     withContext "play-music" [context "artist" "The Beatles"] <| do
--        -- your code here!
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
        (Platform.setTracingSpanDetails (LogContexts contexts))
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
--     Debug.log "Logging a secret" (mkSecret "My PIN is 1234")
--     --> Logging a secret: Secret *****
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

-- | A logged message for log levels warning and above. Because these levels
-- indicate a (potential) problem we want to provide some additional data that
-- would help a triager figure out what next steps to take.
data TriageInfo
  = TriageInfo
      { impact :: Impact,
        advisory :: Text
      }
  deriving (Generic)

instance Aeson.ToJSON TriageInfo

-- | Classification of the levels of impact an issue might have on end-users.
data Impact
  = UserAnnoyed
  | UserConfused
  | UserInPain
  | UserBlocked
  deriving (Show)

instance Aeson.ToJSON Impact where
  toJSON = Aeson.toJSON << impactToText

  toEncoding = Aeson.toEncoding << impactToText

impactToText :: Impact -> Text
impactToText kind =
  case kind of
    UserAnnoyed -> "This is causing inconveniences to users but they will be able to achieve want they want."
    UserBlocked -> "User is blocked from performing an action."
    UserConfused -> "The UI did something unexpected and it's unclear why."
    UserInPain -> "This is causing pain to users and workaround is not obvious."

log :: Stack.HasCallStack => Text -> Bool -> [Context] -> Task e ()
log msg succeeded contexts =
  Internal.tracingSpan msg <| do
    Platform.setTracingSpanDetails (LogContexts contexts)
    if succeeded
      then Task.succeed ()
      else Platform.markTracingSpanFailed
