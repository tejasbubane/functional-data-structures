module BankersQueue where

-- Queue (frontLen frontList rearLen rearList)
data Queue a =
  Queue Int [a] Int [a]
  deriving (Eq, Show)

-- Return empty Queue
empty :: Queue a
empty = Queue 0 [] 0 []

-- Check if given Queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue 0 _ _ _) = True
isEmpty _               = False

-- Return next element to be removed from Queue
head :: Queue a -> Maybe a
head (Queue _ [] _ _) = Nothing
head (Queue _ (f:_) _ _) = Just f

-- Return Queue without head (element to be removed)
tail :: Queue a -> Maybe (Queue a)
tail (Queue _ [] _ _) = Nothing
tail (Queue lenf (f:fs) lenr rs) = Just $ check $ Queue (lenf - 1) fs lenr rs

-- Add elements to rear
snoc :: a -> Queue a -> Queue a
snoc x (Queue _ [] _ _) = Queue 1 [x] 0 []
snoc x (Queue lenf fs lenr rs) = check $ Queue lenf fs (lenr + 1) (x:rs)

-- Ensure the invariant
-- `front` can be empty if and only if `rear` is empty
-- When len(first) <= len(rear) reverse rear and append to front
check :: Queue a -> Queue a
check q@(Queue lenf fs lenr rs)
  | lenf <= lenr = Queue (lenf + lenr) (fs ++ reverse rs) 0 []
  | otherwise = q
