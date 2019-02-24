module Queue where

-- Queue using two lists - front & rear
data Queue a =
  Queue [a] [a]
  deriving (Eq, Show)

-- create an empty Queue
empty :: Queue a
empty = error "todo"

-- check if given queue is empty
isEmpty :: Queue a -> Bool
isEmpty = error "todo"

-- Return next element to be removed from Queue
head :: Queue a -> Maybe a
head = error "todo"

-- Return Queue without head (element to be removed)
tail :: Queue a -> Maybe (Queue a)
tail = error "todo"

-- Add elements to rear
snoc :: a -> Queue a -> Queue a
snoc = error "todo"

-- Ensure the invariant
-- `front` can be empty if and only if `rear` is empty
-- When `front` becomes empty, reverse rear make it the front and make rear empty
check :: [a] -> [a] -> Queue a
check = error "todo"
