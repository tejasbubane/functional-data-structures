module RedBlackTree where

-- Binary Search Trees perform very poorly on ordered data since they become skewed
-- Balanced BST solves this by keeping any operation to max O(log n)
-- Red-Black tree is a popular balanced BST

-- Every node is either red or black
data Color = Red | Black deriving (Eq, Show)

data RBTree a =
  Empty | Node Color (RBTree a) a (RBTree a)
  deriving (Eq, Show)
-- All empty nodes are considered black - so empty does not need color

-- Invariants:
-- 1. Both children of red node should be black
-- 2. Every path from any node to empty node (leaf) contains same number of black nodes

-- member is same as BST (we ignore the colors)
member :: Ord a => a -> RBTree a -> Bool
member = error "todo"

-- insert and delete are different since they must maintain the invariants
insert :: Ord a => a -> RBTree a -> RBTree a
insert = error "todo"

balance :: RBTree a -> RBTree a
balance = error "todo"
