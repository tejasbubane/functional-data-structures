module BankersQueue where

-- Queue (frontLen frontList rearLen rearList)
data Queue a =
  Queue Int [a] Int [a]
  deriving (Eq, Show)

-- Return empty Queue
empty :: Queue a
empty = error "todo"

-- Check if given Queue is empty
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
-- When len(first) <= len(rear) reverse rear and append to front
check :: Queue a -> Queue a
check = error "todo"
