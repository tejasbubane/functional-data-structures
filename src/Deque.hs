module Deque where

import Prelude hiding (head, tail, last, init)

data Deque a =
  Deque Int [a] Int [a]
  deriving (Eq, Show)

empty :: Deque a
empty = Deque 0 [] 0 []

isEmpty :: Deque a -> Bool
isEmpty (Deque 0 [] 0 []) = True
isEmpty _                 = False

-- Add to front
cons :: a -> Deque a -> Deque a
cons x (Deque lenf fs lenr rs) = check $ Deque (lenf + 1) (x:fs) lenr rs

-- Add to rear
snoc :: a -> Deque a -> Deque a
snoc x (Deque lenf fs lenr rs) = check $ Deque lenf fs (lenr + 1) (x:rs)

-- First element from front
head :: Deque a -> a
head (Deque 0 [] 0 []) = error "Deque Empty"
head (Deque 0 [] 1 [x]) = x -- check may leave one element in rear
head (Deque lenf (x:fs) lenr rs) = x

-- Last element (in rear)
last :: Deque a -> a
last (Deque 0 [] 0 []) = error "Deque empty"
last (Deque 1 [x] 0 []) = x -- check may leave one element in front
last (Deque lenf fs lenr (x:rs)) = x

-- Deque without head
tail :: Deque a -> Deque a
tail (Deque 0 [] 0 []) = error "Deque Empty"
tail (Deque 0 [] 1 [x]) = empty -- check may leave one element in rear
tail (Deque lenf (f:fs) lenr rs) = check $ Deque (lenf - 1) fs lenr rs

-- All except last
init :: Deque a -> Deque a
init (Deque 0 [] 0 []) = error "Deque empty"
init (Deque 1 [x] 0 []) = empty -- check may leave one element in front
init (Deque lenf fs lenr (x:rs)) = check $ Deque lenf fs (lenr - 1) rs

check :: Deque a -> Deque a
check dq@(Deque lenf fs lenr rs) =
  if lenf > lenr + 1 then
    let i = (lenf + lenr) `div` 2
        j = lenf + lenr - i
        fs' = take i fs
        rs' = rs ++ reverse (drop j fs)
    in Deque i fs' j rs'
  else if lenr > lenf + 1 then
    let j = (lenf + lenr) `div` 2
        i = lenf + lenr - j
        rs' = take j rs
        fs' = fs ++ reverse (drop j rs)
    in Deque i fs' j rs'
  else dq