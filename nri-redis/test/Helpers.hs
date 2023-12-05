module Helpers where

import qualified Conduit
import qualified Environment
import qualified Redis
import qualified Redis.Handler as Handler
import qualified Redis.Mock as Mock
import qualified Redis.Settings as Settings
import qualified Prelude

data TestHandlers = TestHandlers
  { mockHandler :: Redis.Handler,
    autoExtendExpireHandler :: Redis.HandlerAutoExtendExpire,
    handler :: Redis.Handler
  }

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  autoExtendExpireHandler <- Handler.handlerAutoExtendExpire "tests-auto-extend-expire" settings {Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 1}
  handler <- Handler.handler "tests" settings {Settings.defaultExpiry = Settings.NoDefaultExpiry}
  mockHandler <- Conduit.liftIO <| Mock.handlerIO
  Prelude.pure TestHandlers {mockHandler, autoExtendExpireHandler, handler}
