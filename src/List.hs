module List where

import Prelude hiding (head, tail)

data List a =
  Nil | Cons a (List a)
  deriving (Show, Eq)

empty :: List a
empty = Nil

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _   = False

cons :: a -> List a -> List a
cons x xs = Cons x xs

head :: List a -> a
head Nil = error "empty list"
head (Cons x xs) = x

tail :: List a -> List a
tail Nil = error "empty list"
tail (Cons x xs) = xs

-- Exercises
append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys =
  Cons x (append xs ys)

update :: List a -> a -> Int -> List a
update Nil _ _ = error "index out of bounds"
update (Cons x xs) y 0 = Cons y xs
update (Cons x xs) y n = update xs y (n - 1)
