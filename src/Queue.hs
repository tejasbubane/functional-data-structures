module Queue where

data Queue a =
  Queue [a] [a]
  deriving (Eq, Show)

empty :: Queue a
empty = error "todo"

isEmpty :: Queue a -> Bool
isEmpty = error "todo"

-- remove from front
head :: Queue a -> a
head = error "todo"

tail :: Queue a -> Queue a
tail = error "todo"

-- add to end
snoc :: a -> Queue a -> Queue a
snoc = error "todo"

check :: [a] -> [a] -> Queue a
check = error "todo"
