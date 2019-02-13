import Test.Hspec

import qualified HelloSpec

main :: IO ()
main = hspec $ do
  HelloSpec.specs
