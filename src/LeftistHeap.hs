module LeftistHeap where

data HeapTree a =
  Nil | Node a (HeapTree a) (HeapTree a) Int
  deriving (Eq, Show)
-- Invariant: each node is no larger than all its children
-- and rank(left) >= rank(right)

type Heap = HeapTree

empty :: Heap a
empty = Nil

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _   = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge heap Nil = heap
merge Nil heap = heap
merge h1@(Node x left1 right1 _) h2@(Node y left2 right2 _)
  | x <= y    = makeT x left1 (merge right1 h2)
  | otherwise = makeT y left2 (merge right2 h1)
  where
    rank Nil = 0
    rank (Node _ _ _ i) = i
    makeT el h h' = if rank h >= rank h'
                    then Node el h h' (rank h' + 1) -- right is less ranked
                    else Node el h' h (rank h + 1)

insert :: Ord a => a -> Heap a -> Heap a
insert el h = merge h (newT el)
  where newT x = Node x Nil Nil 1

findMin :: Ord a => Heap a -> Maybe a
findMin Nil = Nothing
findMin (Node el _ _ _) = Just el

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin Nil = Nothing
deleteMin (Node x left right _) = Just $ merge left right
