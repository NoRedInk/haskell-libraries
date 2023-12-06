module Helpers where

import qualified Conduit
import qualified Environment
import qualified Redis
import qualified Redis.Mock as Mock
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Prelude

data TestHandlers = TestHandlers
  { mockHandler :: Redis.Handler,
    autoExtendExpireHandler :: Redis.HandlerAutoExtendExpire,
    realHandler :: Redis.Handler
  }

getHandlers :: Conduit.Acquire TestHandlers
getHandlers = do
  settings <- Conduit.liftIO (Environment.decode Settings.decoder)
  autoExtendExpireHandler <- Real.handlerAutoExtendExpire "tests-auto-extend-expire" settings {Settings.defaultExpiry = Settings.ExpireKeysAfterSeconds 1}
  realHandler <- Real.handler "tests" settings {Settings.defaultExpiry = Settings.NoDefaultExpiry}
  mockHandler <- Conduit.liftIO <| Mock.handlerIO
  Prelude.pure TestHandlers {autoExtendExpireHandler, realHandler, mockHandler}
