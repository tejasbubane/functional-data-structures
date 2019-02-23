module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M

data Trie k v =
  Trie (Maybe v) (M.Map k (Trie k v))
  deriving (Eq, Show)

empty :: Trie k v
empty = Trie Nothing M.empty

-- to lookup string, lookup each character
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup [] (Trie Nothing _)  = Nothing
lookup [] (Trie (Just x) _) = Just x
lookup (k:ks) (Trie _ m) =
  case M.lookup k m of
    Nothing -> Nothing
    Just xs -> lookup ks xs

bind :: Ord k => [k] -> v -> Trie k v -> Trie k v
bind [] x (Trie _ m) = Trie (Just x) m
bind (k:ks) x (Trie v m) =
  case M.lookup k m of
    Nothing -> Trie Nothing $ M.insert k (bind ks x empty) m
    Just t' -> bind ks x t'
