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
member _ Empty = False
member x (Node _ left y right)
  | x == y    = True
  | x < y     = member x left
  | otherwise = member x right

-- insert and delete are different since they must maintain the invariants
insert :: Ord a => a -> RBTree a -> RBTree a
insert x s = blacken $ ins s
  where blacken (Node _ left el right) = Node Black left el right
        ins Empty = Node Red Empty x Empty
        ins s@(Node color left y right)
          -- same as BST except balancing on every step to maintain invariants
          | x < y     = balance $ Node color (ins left) y right
          | x > y     = balance $ Node color left y (ins right)
          | otherwise = s

balance :: RBTree a -> RBTree a
balance (Node Black (Node Red a x (Node Red b y c)) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red (Node Red a x b) y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance node = node
