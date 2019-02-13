import Test.Hspec

import qualified ListSpec

main :: IO ()
main = hspec $ do
  ListSpec.specs
