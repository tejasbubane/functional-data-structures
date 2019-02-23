module DequeSpec where

import Prelude hiding (head, tail, last, init)

import Test.Hspec
import Deque
import Control.Exception (evaluate)

specs :: SpecWith ()
specs = describe "Deque" $ do
  describe "empty" $ do
    it "links empty deque" $ do
      (empty :: Deque Int) `shouldBe` Deque 0 [] 0 []

  describe "isEmpty" $ do
    it "returns True for empty deque" $ do
      isEmpty empty `shouldBe` True
    it "returns False for non-empty deque" $ do
      isEmpty (Deque 0 [] 1 [2]) `shouldBe` False

  describe "cons" $ do
    it "adds given element to empty deque" $ do
      cons 9 empty `shouldBe` Deque 1 [9] 0 []
    it "adds given element to non-empty deque" $ do
      cons 8 (Deque 3 [1,2,3] 4 [4,5,10,9]) `shouldBe`
        Deque 4 [8,1,2,3] 4 [4,5,10,9]

  describe "scons" $ do
    it "adds given element to empty deque" $ do
      snoc 9 empty `shouldBe` Deque 0 [] 1 [9]
    it "adds given element to non-empty deque" $ do
      snoc 8 (Deque 3 [1,2,3] 4 [4,5,10,9]) `shouldBe`
        Deque 4 [1,2,3,9] 4 [8,4,5,10] -- rebalancing in action!

  describe "head" $ do
    it "raises error for empty deque" $ do
      evaluate (head empty) `shouldThrow` anyException
    it "works for single element deque" $ do
      head (Deque 1 [2] 0 []) `shouldBe` 2
      head (Deque 0 [] 1 [4]) `shouldBe` 4
    it "works for multi-element deque" $ do
      head (Deque 2 [2,3] 1 [4]) `shouldBe` 2

  describe "last" $ do
    it "raises error for empty deque" $ do
      evaluate (last empty) `shouldThrow` anyException
    it "works for single element deque" $ do
      last (Deque 1 [2] 0 []) `shouldBe` 2
      last (Deque 0 [] 1 [4]) `shouldBe` 4
    it "works for multi-element deque" $ do
      last (Deque 2 [2,3] 1 [4]) `shouldBe` 4

  describe "tail" $ do
    it "raises error for empty deque" $ do
      evaluate (tail empty) `shouldThrow` anyException
    it "works for single element deque" $ do
      tail (Deque 1 [2] 0 []) `shouldBe` empty
      tail (Deque 0 [] 1 [4]) `shouldBe` empty
    it "works for multi-element deque" $ do
      tail (Deque 4 [2,3,7,0] 5 [9,10,12,11,19]) `shouldBe`
        Deque 4 [3,7,0,19] 4 [9,10,12,11]

  describe "init" $ do
    it "raises error for empty deque" $ do
      evaluate (init empty) `shouldThrow` anyException
    it "works for single element deque" $ do
      init (Deque 1 [2] 0 []) `shouldBe` empty
      init (Deque 0 [] 1 [4]) `shouldBe` empty
    it "works for multi-element deque" $ do
      init (Deque 5 [2,3,7,0,11] 4 [9,10,12,19]) `shouldBe`
        Deque 4 [2,3,7,0] 4 [10,12,19,11]
