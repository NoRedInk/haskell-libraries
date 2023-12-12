import qualified Conduit
import Helpers
import qualified Spec.Redis
import qualified Spec.Settings
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main =
  Conduit.withAcquire Helpers.getHandlers <| \testHandlers ->
    Test.run
      <| Test.describe
        "nri-redis"
        [ Spec.Redis.tests testHandlers,
          Spec.Settings.tests
        ]
