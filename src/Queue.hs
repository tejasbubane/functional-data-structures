module Queue where

data Queue a =
  Queue [a] [a]
  deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue f _) = null f

-- remove from front
head :: Queue a -> Maybe a
head (Queue [] _)     = Nothing
head (Queue (x:f) _) = Just x

tail :: Queue a -> Maybe (Queue a)
tail (Queue [] _) = Nothing
tail (Queue (x:f) r) = Just $ checkf f r

-- add to end
snoc :: a -> Queue a -> Queue a
snoc x (Queue [] _) = Queue [x] [] -- r will always be empty if f is empty (that's the invariant)
snoc x (Queue f r) = Queue f (x:r)

checkf :: [a] -> [a] -> Queue a
checkf [] r = Queue (reverse r) []
checkf f r  = Queue f r
