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

-- Exercises
append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys =
  Cons x (append xs ys)

update :: List a -> a -> Int -> Maybe (List a)
update Nil _ _ = Nothing
update (Cons _ _) _ 0 = Nothing
update (Cons x xs) y 1 = Just $ Cons y xs
update (Cons x xs) y n = update xs y (n - 1)
