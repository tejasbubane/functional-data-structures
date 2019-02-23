module BankersQueue where

data Queue a =
  Queue Int [a] Int [a]
  deriving (Eq, Show)

empty :: Queue a
empty = error "todo"

isEmpty :: Queue a -> Bool
isEmpty = error "todo"

head :: Queue a -> a
head = error "todo"

tail :: Queue a -> Queue a
tail = error "todo"

snoc :: a -> Queue a -> Queue a
snoc = error "todo"

check :: Queue a -> Queue a
check = error "todo"
