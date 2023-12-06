import qualified Conduit
import Helpers
import qualified Spec.Redis
import qualified Test
import qualified Prelude

main :: Prelude.IO ()
main = Conduit.withAcquire Helpers.getHandlers (Test.run << Spec.Redis.tests)
