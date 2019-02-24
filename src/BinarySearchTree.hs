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
empty = error "todo"

-- Insert element in BST
insert :: Ord a => a -> Set a -> Set a
insert = error "todo"

-- Check if given element exists in subtree
member :: Ord a => a -> Set a -> Bool
member = error "todo"

-- Apply given function to all elements of BST without changing the structure
instance Functor Tree where
  fmap f Nil = error "todo"
  fmap f (Node left x right) = error "todo"
