import qualified ServantSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Servant" ServantSpec.spec
