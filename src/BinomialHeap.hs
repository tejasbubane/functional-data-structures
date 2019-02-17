module BinomialHeap where

-- node with rank, element & list of children
data BinTree a =
  Node Int a [BinTree a]
  deriving (Eq, Show)

-- Binomial tree of rank 0 is singleton
-- Of rank r + 1 is formed by linking two binomial trees of tank r,
-- making one tree the lleftmost child of the other

link :: Ord a => BinTree a -> BinTree a -> BinTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2:c1)
  | otherwise = Node (r + 1) x2 (t1:c2)
-- Assumption is we always link nodes of same rank

-- Heap of n items is analogous to (log n) trees
-- eg. 13 == 1101 -> binomial heap with 13 nodes will consist of three
-- binomial trees of 3, 2, 0
type Heap a = [BinTree a]

rank :: BinTree a -> Int
rank (Node r _ _) = r

insert :: Ord a => BinTree a -> Heap a -> Heap a
insert t [] = [t]
insert t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insert (link t t') ts' -- analogous to carry in binary addition

-- insert can be thought of as adding 1 to binary number
-- if last is 1, 1 + 1 = 0 and carry 1 (carry == link) - keep doing this
-- (heap list is ordered min to max)

-- And merge is like adding two binary numbers
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(t1:ts1) h2@(t2:ts2)
  | rank t1 < rank t2 = (t1:(merge ts1 h2))
  | rank t2 < rank t1 = (t2:(merge h1 ts2))
  | otherwise         = insert (link t1 t2) (merge ts1 ts2)

root :: BinTree a -> a
root (Node _ el _) = el

removeMinTree :: Ord a => Heap a -> (BinTree a, Heap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  let (t', ts') = removeMinTree ts
  in
    if root t <= root t'
    then (t, ts)
    else (t', (t:ts'))

findMin :: Ord a => Heap a -> a
findMin = root . fst . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts =
  let ((Node _ _ ts1), ts2) = removeMinTree ts
  in merge (reverse ts1) ts2
