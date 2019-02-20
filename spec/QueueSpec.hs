module QueueSpec where

import Test.Hspec
import Queue
import Prelude hiding (head, tail)

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
    it "is Nothing for empty queue" $ do
      head (empty :: Queue Int) `shouldBe` Nothing
    it "is the first element of f for non-empty queue" $ do
      head (Queue [1] []) `shouldBe` Just 1
      head (Queue [2, 3] [1, 5]) `shouldBe` Just 2

  describe "tail" $ do
    it "is Nothing for empty queue" $ do
      tail (empty :: Queue Int) `shouldBe` Nothing
    it "is everything but the first element for non-empty queue" $ do
      tail (Queue [1] []) `shouldBe` Just empty
      tail (Queue [2, 3] [1, 5]) `shouldBe` Just (Queue [3] [1, 5])
    it "reverses rear to front when front becomes empty (invariant)" $ do
      tail (Queue [2] [1, 5]) `shouldBe` Just (Queue [5, 1] [])

  describe "snoc" $ do
    it "adds element to end of queue" $ do
      snoc 10 (Queue [2] [1, 5]) `shouldBe` Queue [2] [10, 1, 5]
    it "reverses rear to front when front becomes empty (invariant)" $ do
      snoc 2 (empty :: Queue Int) `shouldBe` Queue [2] []
