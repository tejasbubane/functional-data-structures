module RedBlackTreeSpec where

import Test.Hspec
import RedBlackTree

leaf :: Int -> RBTree Int
leaf x = Node Black Empty x Empty

-- single red node
rleaf :: Int -> RBTree Int
rleaf x = Node Red Empty x Empty

specs :: SpecWith ()
specs = describe "Red-Black Tree" $ do
  describe "member" $ do
    it "returns False for empty tree" $ do
      member 1 Empty `shouldBe` False
    it "checks if given element is present in the Tree" $ do
      member 3 (Node Black Empty 3 Empty) `shouldBe` True
      member 4 (Node Black (leaf 2) 3 (leaf 12)) `shouldBe` False
      member 12 (Node Black (leaf 2) 3 (leaf 12)) `shouldBe` True

  describe "insert" $ do
    it "adds element in empty tree" $ do
      insert 2 Empty `shouldBe` leaf 2
    it "adds element in non-empty tree" $ do
      insert 10 (Node Black (rleaf 2) 12 (rleaf 34)) `shouldBe`
        Node Black (leaf 2) 10 (Node Black Empty 12 (rleaf 34))
