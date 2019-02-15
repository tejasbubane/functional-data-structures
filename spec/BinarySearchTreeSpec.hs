module BinarySearchTreeSpec where

import Test.Hspec
import BinarySearchTree

specs :: SpecWith ()
specs = describe "Binary Search Tree" $ do
  describe "empty" $ do
    it "returns empty set" $ do
      empty `shouldBe` (Nil :: Tree Int)

  describe "insert" $ do
    it "adds element to empty set" $ do
      insert 2 Nil `shouldBe` Node Nil 2 Nil
    it "adds element to non-empty set at right position" $ do
      insert 10 (Node (Node Nil 5 Nil) 7 (Node Nil 9 Nil)) `shouldBe`
        (Node (Node Nil 5 Nil) 7 (Node Nil 9 (Node Nil 10 Nil)))

  describe "member" $ do
    it "returns False for empty set" $ do
      member 1 Nil `shouldBe` False
    it "checks if member in non-empty set" $ do
      member 6 (Node (Node Nil 5 Nil) 7 (Node Nil 10 Nil)) `shouldBe` False
      member 10 (Node (Node Nil 5 Nil) 7 (Node Nil 10 Nil)) `shouldBe` True
