module QueueSpec where

import Test.Hspec
import Queue
import Prelude hiding (head, tail)
import Control.Exception (evaluate)

specs :: SpecWith ()
specs = describe "Queue" $ do
  describe "empty" $ do
    it "creates empty queue" $ do
      (empty :: Queue Int) `shouldBe` Queue [] []

  describe "isEmpty" $ do
    it "is True for empty queue" $ do
      isEmpty empty `shouldBe` True
      isEmpty (Queue [1] []) `shouldBe` False
      isEmpty (Queue [2, 3] [1, 5]) `shouldBe` False

  describe "head" $ do
    it "raises exception for empty queue" $ do
      evaluate (head (empty :: Queue Int)) `shouldThrow` anyException
    it "is the first element of f for non-empty queue" $ do
      head (Queue [1] []) `shouldBe` 1
      head (Queue [2, 3] [1, 5]) `shouldBe` 2

  describe "tail" $ do
    it "raises exception for empty queue" $ do
      evaluate (tail (empty :: Queue Int)) `shouldThrow` anyException
    it "is everything but the first element for non-empty queue" $ do
      tail (Queue [1] []) `shouldBe` empty
      tail (Queue [2, 3] [1, 5]) `shouldBe` Queue [3] [1, 5]
    it "reverses rear to front when front becomes empty (invariant)" $ do
      tail (Queue [2] [1, 5]) `shouldBe` Queue [5, 1] []

  describe "snoc" $ do
    it "adds element to end of queue" $ do
      snoc 10 (Queue [2] [1, 5]) `shouldBe` Queue [2] [10, 1, 5]
    it "reverses rear to front when front becomes empty (invariant)" $ do
      snoc 2 (empty :: Queue Int) `shouldBe` Queue [2] []
