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

hd :: List a -> a
hd Nil = error "empty list"
hd (Cons x xs) = x

tl :: List a -> List a
tl Nil = error "empty list"
tl (Cons x xs) = xs

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
