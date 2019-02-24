module List where

import Prelude hiding (head, tail)
import Data.Maybe (fromMaybe)

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

head :: List a -> Maybe a
head Nil = Nothing
head (Cons x xs) = Just x

tail :: List a -> Maybe (List a)
tail Nil = Nothing
tail (Cons x xs) = Just xs

-- Exercises
append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys =
  Cons x (append xs ys)

update :: List a -> a -> Int -> Maybe (List a)
update Nil _ _ = Nothing
update (Cons x xs) y 0 = Just $ Cons y xs
update (Cons x xs) y n =
  case update xs y (n - 1) of
    Nothing -> Nothing
    Just xs' -> Just $ Cons x xs'
