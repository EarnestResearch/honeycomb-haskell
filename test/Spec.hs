import qualified ApiSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Api" ApiSpec.spec
