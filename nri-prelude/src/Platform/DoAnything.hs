module Platform.DoAnything
  ( Handler (Handler),
  )
where

-- |
-- tldr; Having a value of this type represents an allowance to run any @IO@ in
-- the @Task@ type. Intended for use in effects packages only.
--
-- Longer explanation:
--
-- One of our goals with creating a shared @Task@ type is that our libraries
-- can all use it. Instead of returning polymorphic values like:
--
--   (MonadError e m, Monad IO m) => m a
--
-- We could instead let our libraries return @Task e m@. Looks simpler, no?
--
-- But for this to fly our libraries will need to be able to run @IO@ in
-- @Task@. The postgres library for example will want to talk to the
-- database. How should it take a @IO a@ returned from @postgresql-typed@
-- and turn it into an @Task e a@ to return to the library user?
--
-- @MonadIO@ is intended for this stuff. By deriving it for our @Task@ type
-- we could use @liftIO@ to take an @IO a@ value and turn it into @Task e a@.
-- But any part of the code could do this, and we'd like to restrict
-- ourselves from running IO in our applications to functions that take a
-- handler, indicating the type of effect they perform.
--
-- This commit uses the handler pattern to restrict access to arbitrary IO
-- in @Task@: You can turn an @IO a@ into a @Task e a@, but only if you can
-- submit a @Handler@ to proof you're allowed. Are libraries will
-- be allowed, but our applications not.
--
-- You can only get a handler wrapped in an @IO@. This means you
-- can only use it in an environment where you can perform arbitrary IO. We
-- cannot extract the value from a @Data.Acquire@ in our @Task@ code, but we can
-- use it to generate handlers for other packages. The Postgres package for
-- example can store the @Handler@ in its @Connection@ handler, and
-- so whenever Postgres is passed a @Connection@ to perform a query, it also
-- has access to the @Handler@ necessary to perform IO.
data Handler
  = Handler
