module LeftistHeap where

data HeapTree a =
  Nil | Node a (HeapTree a) (HeapTree a) Int
  deriving (Eq, Show)
-- Invariant: each node is no larger than all its children
-- and rank(left) >= rank(right)

type Heap = HeapTree

empty :: Heap a
empty = error "todo"

isEmpty :: Heap a -> Bool
isEmpty = error "todo"

merge :: Ord a => Heap a -> Heap a -> Heap a
merge = error "todo"

insert :: Ord a => a -> Heap a -> Heap a
insert = error "todo"

findMin :: Ord a => Heap a -> a
findMin = error "todo"

deleteMin :: Ord a => Heap a -> Heap a
deleteMin = error "todo"

-- Exercise: Build heap from list
fromList :: Ord a => [a] -> Heap a
fromList = error "todo"
