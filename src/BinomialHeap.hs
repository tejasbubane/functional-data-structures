module BinomialHeap where

-- node with rank, element & list of children
data BinTree a =
  Node Int a [BinTree a]
  deriving (Eq, Show)

-- Binomial tree of rank 0 is singleton
-- Of rank r + 1 is formed by linking two binomial trees of tank r,
-- making one tree the lleftmost child of the other

link :: Ord a => BinTree a -> BinTree a -> BinTree a
link = error "todo"
-- Assumption is we always link nodes of same rank

-- Heap of n items is analogous to (log n) trees
-- eg. 13 == 1101 -> binomial heap with 13 nodes will consist of three
-- binomial trees of 3, 2, 0
type Heap a = [BinTree a]

rank :: BinTree a -> Int
rank = error "todo"

insert :: Ord a => BinTree a -> Heap a -> Heap a
insert = error "todo"

-- insert can be thought of as adding 1 to binary number
-- if last is 1, 1 + 1 = 0 and carry 1 (carry == link) - keep doing this
-- (heap list is ordered min to max)

-- And merge is like adding two binary numbers
merge :: Ord a => Heap a -> Heap a -> Heap a
merge = error "todo"

removeMinTree :: Ord a => Heap a -> (BinTree a, Heap a)
removeMinTree = error "todo"

findMin :: Ord a => Heap a -> a
findMin = error "todo"

deleteMin :: Ord a => Heap a -> Heap a
deleteMin = error "todo"
