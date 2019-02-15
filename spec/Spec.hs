import Test.Hspec

import qualified ListSpec
import qualified BinarySearchTreeSpec
import qualified LeftistHeapSpec

main :: IO ()
main = hspec $ do
  ListSpec.specs
  BinarySearchTreeSpec.specs
  LeftistHeapSpec.specs
