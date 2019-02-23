module List where

data List a =
  Nil | Cons a (List a)
  deriving (Show, Eq)

empty :: List a
empty = error "todo"

isEmpty :: List a -> Bool
isEmpty = error "todo"

cons :: a -> List a -> List a
cons = error "todo"

hd :: List a -> a
hd = error "todo"

tl :: List a -> List a
tl = error "todo"

-- Exercises
append :: List a -> List a -> List a
append = error "todo"

update :: List a -> a -> Int -> List a
update = error "todo"
