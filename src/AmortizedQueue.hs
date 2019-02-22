module AmortizedQueue where

data Queue a =
  Queue Int [a] Int [a]
  deriving (Eq, Show)

empty :: Queue a
empty = Queue 0 [] 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue 0 _ _ _) = True
isEmpty _               = False

head :: Queue a -> Maybe a
head (Queue _ [] _ _) = Nothing
head (Queue _ (f:_) _ _) = Just f

tail :: Queue a -> Maybe (Queue a)
tail (Queue _ [] _ _) = Nothing
tail (Queue lenf (f:fs) lenr rs) = Just $ check $ Queue (lenf - 1) fs lenr rs

snoc :: a -> Queue a -> Queue a
snoc x (Queue _ [] _ _) = Queue 1 [x] 0 []
snoc x (Queue lenf fs lenr rs) = check $ Queue lenf fs (lenr + 1) (x:rs)

check :: Queue a -> Queue a
check q@(Queue lenf fs lenr rs)
  | lenf <= lenr = Queue (lenf + lenr) (fs ++ reverse rs) 0 []
  | otherwise = q
