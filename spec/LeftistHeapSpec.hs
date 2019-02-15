module LeftistHeapSpec where

import Test.Hspec
import LeftistHeap

singleton :: a -> Heap a
singleton x = Node x Nil Nil 1

-- Some sample trees to test against
h :: HeapTree Int
h = Node 5 (singleton 6) (Node 12 (singleton 15) Nil 1) 2
h' :: HeapTree Int
h' = Node 3 (Node 7 (singleton 14) Nil 1) (Node 8 (singleton 11) Nil 1) 2

specs :: SpecWith ()
specs = describe "Heap" $ do
  describe "empty" $ do
    it "returns empty heap" $ do
      empty `shouldBe` (Nil :: Heap Int)

  describe "isEmpty" $ do
    it "returns True for empty" $ do
      isEmpty Nil `shouldBe` True
    it "returns False for empty" $ do
      isEmpty (singleton 4) `shouldBe` False
      isEmpty (Node 1 (singleton 3) Nil 1) `shouldBe` False

  describe "merge" $ do
    it "merges with any empty heap" $ do
      merge (Nil :: Heap Int) (Nil :: Heap Int) `shouldBe` Nil
      merge Nil (singleton 3) `shouldBe` (singleton 3)
      merge (singleton 4) Nil `shouldBe` (singleton 4)
    it "merges with non-empty heaps" $ do
      let right = Node 7 (singleton 14) Nil 1
          left = Node 5 (Node 8 (singleton 11) (Node 12 (singleton 15) Nil 1) 2)
                        (singleton 6) 2
      merge h h' `shouldBe` (Node 3 left right 2)

  describe "insert" $ do
    it "inserts into empty heap" $ do
      insert 2 Nil `shouldBe` singleton 2
    it "inserts into non-empty heap" $ do
      insert 10 h `shouldBe`
        Node 5 (singleton 6) (Node 10 (Node 12 (singleton 15) Nil 1) Nil 1) 2

  describe "findMin" $ do
    it "returns Nothing for empty heap" $ do
      findMin (Nil :: Heap Int) `shouldBe` Nothing
    it "returns topmost element for non-empty heap" $ do
      findMin (singleton 3) `shouldBe` Just 3
      findMin (Node 1 (singleton 7) (singleton 4) 2) `shouldBe` Just 1

  describe "deleteMin" $ do
    it "returns Nothing for empty heap" $ do
      deleteMin (Nil :: Heap Int) `shouldBe` Nothing
    it "returns Nil for single element heap" $ do
      deleteMin (singleton 4) `shouldBe` Just Nil
    it "returns Heap of remainining elements for non-empty heap" $ do
      deleteMin h `shouldBe` Just (Node 6 (Node 12 (singleton 15) Nil 1) Nil 1)
