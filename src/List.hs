module List where

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

hd :: List a -> Maybe a
hd Nil = Nothing
hd (Cons x xs) = Just x

tl :: List a -> Maybe (List a)
tl Nil = Nothing
tl (Cons x xs) = Just xs
