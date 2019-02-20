import Test.Hspec

import qualified ListSpec
import qualified BinarySearchTreeSpec
import qualified LeftistHeapSpec
import qualified BinomialHeapSpec
import qualified RedBlackTreeSpec
import qualified QueueSpec

main :: IO ()
main = hspec $ do
  ListSpec.specs
  BinarySearchTreeSpec.specs
  LeftistHeapSpec.specs
  BinomialHeapSpec.specs
  RedBlackTreeSpec.specs
  QueueSpec.specs
