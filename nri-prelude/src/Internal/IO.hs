{-# LANGUAGE BlockArguments #-}

module Internal.IO
  ( putText,
    putTextLn,
  )
where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import Text
import qualified Prelude

{-# NOINLINE stdoutLock #-}
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO (newMVar ())

-- Output functions

-- | Write text to stdout.
--
-- This function is similar to `Prelude.putStr` but receives a `Text` instead of a `Prelude.String`.
-- Also this function is thread-safe, using a lock to prevent interleaved output.
putText :: Text -> Prelude.IO ()
putText text = do
  withMVar stdoutLock \_ ->
    Prelude.putStr (Text.toList text)

-- | Write text to stdout followed by a newline.
--
-- This function is similar to `Prelude.putStrLn` but receives a `Text` instead of a `Prelude.String`.
-- Also this function is thread-safe, using a lock to prevent interleaved output.
putTextLn :: Text -> Prelude.IO ()
putTextLn text = do
  withMVar stdoutLock \_ ->
    Prelude.putStrLn (Text.toList text)
