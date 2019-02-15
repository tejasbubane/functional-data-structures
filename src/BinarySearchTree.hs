module BinarySearchTree where

data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

type Set = Tree

empty :: Set a
empty = error "todo"

insert :: Ord a => a -> Set a -> Set a
insert = error "todo"

member :: Ord a => a -> Set a -> Bool
member = error "todo"
