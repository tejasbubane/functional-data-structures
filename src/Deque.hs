module Deque where

import Prelude hiding (head, tail, last, init)

-- Double ended queues
-- Adding and removing allowed from both ends
data Deque a =
  Deque Int [a] Int [a]
  deriving (Eq, Show)

empty :: Deque a
empty = error "todo"

isEmpty :: Deque a -> Bool
isEmpty = error "todo"

-- Add to front
cons :: a -> Deque a -> Deque a
cons = error "todo"

-- Add to rear
snoc :: a -> Deque a -> Deque a
snoc = error "todo"

-- First element from front
head :: Deque a -> a
head = error "todo"

-- Last element (in rear)
last :: Deque a -> a
last = error "todo"

-- Deque without head
tail :: Deque a -> Deque a
tail = error "todo"

-- All except last
init :: Deque a -> Deque a
init = error "todo"

check :: Deque a -> Deque a
check = error "todo"
