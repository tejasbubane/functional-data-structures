module AmortizedQueueSpec where

import Test.Hspec
import AmortizedQueue
import Prelude hiding (head, tail)
import Control.Exception (evaluate)

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
    it "raises exception for empty queue" $ do
      evaluate (head (empty :: Queue Int)) `shouldThrow` anyException
    it "is the first element of f for non-empty queue" $ do
      head (Queue 1 [1] 0 []) `shouldBe` 1
      head (Queue 2 [2, 3] 2 [1, 5]) `shouldBe` 2

  describe "tail" $ do
    it "raises exception for empty queue" $ do
      evaluate (tail (empty :: Queue Int)) `shouldThrow` anyException
    it "is everything but the first element for non-empty queue" $ do
      tail (Queue 1 [1] 0 []) `shouldBe` empty
      tail (Queue 2 [2, 3] 2 [1, 5]) `shouldBe` Queue 3 [3, 5, 1] 0 []

  describe "snoc" $ do
    it "adds element to end of queue" $ do
      snoc 10 (Queue 2 [2, 3] 2 [1, 5]) `shouldBe`
        Queue 5 [2, 3, 5, 1, 10] 0 []
