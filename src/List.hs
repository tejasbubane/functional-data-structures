module List where

import Prelude hiding (head, tail)

data List a =
  Nil | Cons a (List a)
  deriving (Show, Eq)

empty :: List a
empty = error "todo"

isEmpty :: List a -> Bool
isEmpty = error "todo"

cons :: a -> List a -> List a
cons = error "todo"

head :: List a -> Maybe a
head = error "todo"

tail :: List a -> Maybe (List a)
tail = error "todo"

-- Exercises
append :: List a -> List a -> List a
append = error "todo"

update :: List a -> a -> Int -> Maybe (List a)
update = error "todo"
