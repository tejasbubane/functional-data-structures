module Queue where

-- Queue using two lists - front & rear
data Queue a =
  Queue [a] [a]
  deriving (Eq, Show)

-- create an empty Queue
empty :: Queue a
empty = Queue [] []

-- check if given queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue f _) = null f

-- Return next element to be removed from Queue
head :: Queue a -> Maybe a
head (Queue [] _)    = Nothing
head (Queue (x:f) _) = Just x

-- Return Queue without head (element to be removed)
tail :: Queue a -> Maybe (Queue a)
tail (Queue [] _) = Nothing
tail (Queue (x:f) r) = Just $ checkf f r

-- Add elements to rear
snoc :: a -> Queue a -> Queue a
snoc x (Queue [] _) = Queue [x] [] -- r will always be empty if f is empty (that's the invariant)
snoc x (Queue f r) = Queue f (x:r)

-- Ensure the invariant
-- `front` can be empty if and only if `rear` is empty
-- When `front` becomes empty, reverse rear make it the front and make rear empty
checkf :: [a] -> [a] -> Queue a
checkf [] r = Queue (reverse r) []
checkf f r  = Queue f r
