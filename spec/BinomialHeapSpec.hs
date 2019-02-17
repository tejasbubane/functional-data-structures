module BinomialHeapSpec where

import Test.Hspec
import BinomialHeap

-- create Rank 1 tree
tree1 :: Int -> BinTree Int
tree1 x = (Node 1 x [(Node 0 (x+1) [])])

-- create Rank 2 tree
tree2 :: Int -> BinTree Int
tree2 x = (Node 2 x [(tree1 (x + 1)), (Node 0 (x + 2) [])])

specs :: SpecWith ()
specs = describe "Binomial Heap" $ do
  describe "link" $ do
    it "links singleton nodes" $ do
      link (Node 0 1 []) (Node 0 2 []) `shouldBe` tree1 1
    it "links rank n nodes" $ do
      link (tree1 11) (tree1 14) `shouldBe`
        Node 2 11 [(tree1 14), (Node 0 12 [])]
      link (tree2 5) (tree2 12) `shouldBe`
        Node 3 5 [(tree2 12), (tree1 6), (Node 0 7 [])]

  describe "insert" $ do
    it "inserts into empty heap" $ do
      insert (tree1 2) [] `shouldBe` [(tree1 2)]
    it "inserts into non-empty heap" $ do
      insert (tree1 11) [(tree1 14)] `shouldBe`
        [Node 2 11 [(tree1 14), (Node 0 12 [])]]

  describe "merge" $ do
    let empty = [] :: Heap Int
    it "merges empty heaps" $ do
      merge empty empty `shouldBe` empty
      merge [(tree1 2)] empty `shouldBe` [(tree1 2)]
      merge empty [(tree2 9)] `shouldBe` [(tree2 9)]
    it "merges non-empty heaps" $ do
      merge [(tree1 11)] [(tree1 14)] `shouldBe`
        [Node 2 11 [(tree1 14), (Node 0 12 [])]]

  describe "findMin" $ do
    it "finds min from non-empty heap" $ do
      findMin [(tree1 3), (Node 0 1 [])] `shouldBe` 1
      findMin [(tree2 2), (Node 0 11 [])] `shouldBe` 2

  describe "deleteMin" $ do
    it "deletes root of extracted tree" $ do
      deleteMin [(tree1 3), (Node 0 1 [])] `shouldBe` [(tree1 3)]
