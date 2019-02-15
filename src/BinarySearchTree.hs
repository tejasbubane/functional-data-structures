module BinarySearchTree where

data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

type Set = Tree

empty :: Set a
empty = Nil

insert :: Ord a => a -> Set a -> Set a
insert x Nil = Node Nil x Nil
insert x tree@(Node left y right)
  | x < y = Node (insert x left) y right
  | x > y = (Node left y (insert x right))
  | otherwise = tree

member :: Ord a => a -> Set a -> Bool
member x Nil = False
member x (Node left y right)
  | x == y = True
  | x < y  = member x left
  | x > y  = member x right

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)
