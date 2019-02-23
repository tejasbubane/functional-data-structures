module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M

data Trie k v =
  Trie (Maybe v) (M.Map k (Trie k v))
  deriving (Eq, Show)

empty :: Trie k v
empty = error "todo"

-- to lookup string, lookup each character
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup = error "todo"

bind :: Ord k => [k] -> v -> Trie k v -> Trie k v
bind = error "todo"
