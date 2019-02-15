import Test.Hspec

import qualified ListSpec
import qualified BinarySearchTreeSpec

main :: IO ()
main = hspec $ do
  ListSpec.specs
  BinarySearchTreeSpec.specs
