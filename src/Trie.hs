module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M

-- https://en.wikipedia.org/wiki/Trie
-- Consider Trie as a key(k) value(v) pair
-- k & v can be different types (unlike most other structures in this repo which are homogenous)
-- Value node may or may not have a value (hence Maybe v)
-- children of every node represent path-label and subtree in a HashMap
-- Tries can be defined in terms of themselves - but that gets hard quickly to satisfy types
-- Hence we use inbuilt Maps
data Trie k v =
  Trie (Maybe v) (M.Map k (Trie k v))
  deriving (Eq, Show)

-- create empty Trie
empty :: Trie k v
empty = error "todo"

-- to lookup string, lookup each character
-- Fail fast (as soon as any path of key not found)
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup = error "todo"

-- Add element to Trie
-- Note if key not found, we need to create the path
bind :: Ord k => [k] -> v -> Trie k v -> Trie k v
bind = error "todo"
