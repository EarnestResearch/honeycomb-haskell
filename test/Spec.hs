import qualified ApiSpec
import qualified HoneycombSpec
import Test.Hspec
import qualified TraceSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Api" ApiSpec.spec
  describe "Honeycomb" HoneycombSpec.spec
  describe "Trace" TraceSpec.spec
