module BinarySearchTreeSpec where

import Test.Hspec
import BinarySearchTree
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    left <- arbitrary
    right <- arbitrary
    oneof [return $ Nil
          , return $ Node left a right]

instance Eq a => EqProp (Tree a) where (=-=) = eq

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
      member 7 (Node (Node Nil 5 Nil) 7 (Node Nil 10 Nil)) `shouldBe` True

  describe "functor" $ do
    it "follows the functor laws" $ do
      property $ quickBatch $ functor (undefined :: Tree (Int, Int, Int))
