module HelloSpec where

import Test.Hspec
import Hello

specs :: SpecWith ()
specs = describe "Hello" $ do
  it "works" $ do
    plusOne 2 `shouldBe` 3
