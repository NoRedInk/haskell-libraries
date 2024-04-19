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

putText :: Text -> Prelude.IO ()
putText text = do
  withMVar stdoutLock \_ ->
    Prelude.putStr (Text.toList text)

putTextLn :: Text -> Prelude.IO ()
putTextLn text = do
  withMVar stdoutLock \_ ->
    Prelude.putStrLn (Text.toList text)
