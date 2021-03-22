-- | This module can be useful while /developing/ an application. It should not
-- be used in production.
module Debug
  ( -- * Debugging
    toString,
    log,
    todo,
  )
where

import Basics ((>>))
import Data.Text (pack, unpack)
import qualified Debug.Trace
import qualified GHC.Stack as Stack
import Text (Text, concat)
import qualified Text.Show.Pretty
import Prelude (Show, error)

-- | Turn any @Show@able kind of value into a string.
--
-- > toString 42                == "42"
-- > toString [1,2]             == "[1,2]"
-- > toString ('a', "cat", 13)  == "('a', \"cat\", 13)"
-- > toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""
--
-- Notice that with strings, this is not the @identity@ function. Ultimately it's
-- down to the value's @Show@ instance, but for strings this typically escapes
-- characters. If you say @toString "he said, \\"hi\\""@ it will show @"he said,
-- \\"hi\\""@ rather than @he said, "hi"@.
toString :: Show a => a -> Text
toString =
  Text.Show.Pretty.ppShow >> pack

-- | Log a tagged value on the developer console, and then return the value.
--
-- > 1 + log "number" 1        -- equals 2, logs "number: 1"
-- > length (log "start" [])   -- equals 0, logs "start: []"
--
-- It is often possible to sprinkle this around to see if values are what you
-- expect. It is kind of old-school to do it this way, but it works!
log :: Show a => Text -> a -> a
log message value =
  Debug.Trace.trace (unpack (concat [message, ": ", toString value])) value

-- | This is a placeholder for code that you will write later.
--
-- For example, if you are working with a large union type and have partially
-- completed a case expression, it may make sense to do this:
--
-- > type Entity = Ship | Fish | Captain | Seagull
-- >
-- > drawEntity entity =
-- >   case entity of
-- >     Ship ->
-- >       ...
-- >
-- >     Fish ->
-- >       ...
-- >
-- >     _ ->
-- >       Debug.todo "handle Captain and Seagull"
--
-- When you call this it throws an exception with the message you give. That
-- exception is catchable... but don't.
todo :: Stack.HasCallStack => Text -> a
todo =
  Stack.withFrozenCallStack (unpack >> error)
