module AmortizedQueueSpec where

import Test.Hspec
import AmortizedQueue
import Prelude hiding (head, tail)

specs :: SpecWith ()
specs = describe "Amortized Queue" $ do
  describe "empty" $ do
    it "creates empty queue" $ do
      (empty :: Queue Int) `shouldBe` Queue 0 [] 0 []

  describe "isEmpty" $ do
    it "is True for empty queue" $ do
      isEmpty empty `shouldBe` True
      isEmpty (Queue 1 [1] 0 []) `shouldBe` False
      isEmpty (Queue 2 [2, 3] 2 [1, 5]) `shouldBe` False

  describe "head" $ do
    it "is Nothing for empty queue" $ do
      head (empty :: Queue Int) `shouldBe` Nothing
    it "is the first element of f for non-empty queue" $ do
      head (Queue 1 [1] 0 []) `shouldBe` Just 1
      head (Queue 2 [2, 3] 2 [1, 5]) `shouldBe` Just 2

  describe "tail" $ do
    it "is Nothing for empty queue" $ do
      tail (empty :: Queue Int) `shouldBe` Nothing
    it "is everything but the first element for non-empty queue" $ do
      tail (Queue 1 [1] 0 []) `shouldBe` Just empty
      tail (Queue 2 [2, 3] 2 [1, 5]) `shouldBe` Just (Queue 3 [3, 5, 1] 0 [])

  describe "snoc" $ do
    it "adds element to end of queue" $ do
      snoc 10 (Queue 2 [2, 3] 2 [1, 5]) `shouldBe`
        Queue 5 [2, 3, 5, 1, 10] 0 []
