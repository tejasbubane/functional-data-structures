module BinarySearchTree where

-- Binary Search Tree (BST)
-- Nil is for empty bottom nodes
-- Node (left-subtree) element (right-subtree)
data Tree a =
  Nil | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Invariant: All elements in left subtree are less than root
-- while all elements in right subtree are greater than root

type Set = Tree
-- Since trees allow fast O(log n) inserts and lookups,
-- they can be used to implement Sets

-- return empty tree
empty :: Set a
empty = Nil

-- Insert element in BST
insert :: Ord a => a -> Set a -> Set a
insert x Nil = Node Nil x Nil
insert x tree@(Node left y right)
  | x < y = Node (insert x left) y right
  | x > y = (Node left y (insert x right))
  | otherwise = tree

-- Check if given element exists in subtree
member :: Ord a => a -> Set a -> Bool
member x Nil = False
member x (Node left y right)
  | x == y = True
  | x < y  = member x left
  | x > y  = member x right

-- Apply given function to all elements of BST without changing the structure
instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)
