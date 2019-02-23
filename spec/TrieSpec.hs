module TrieSpec where

import Test.Hspec
import Trie
import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Exception (evaluate)

trie :: Trie Char Int
trie = Trie Nothing $
       M.fromList [('c', Trie Nothing $
                     M.fromList [('a', Trie Nothing $
                                   M.fromList [('t', Trie (Just 123) M.empty)])])]

specs :: SpecWith ()
specs = describe "Trie" $ do
  describe "empty" $ do
    it "creates empty trie" $ do
      (empty :: Trie Char Int) `shouldBe` Trie Nothing M.empty

  describe "lookup" $ do
    it "returns Nothing on empty trie" $ do
      lookup "str" (empty :: Trie Char Int) `shouldBe` Nothing
    it "returns Nothing when lookup fails" $ do
      lookup "str" (empty :: Trie Char Int) `shouldBe` Nothing
    it "returns found value on successful lookup" $ do
      lookup "cat" trie `shouldBe` Just 123

  describe "bind" $ do
    it "binds on empty trie" $ do
      lookup "cat" (bind "cat" 234 empty) `shouldBe` Just 234
    it "binds on non-empty trie" $ do
      lookup "boat" (bind "boat" 967 trie) `shouldBe` Just 967
