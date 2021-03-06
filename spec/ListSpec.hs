module ListSpec where

import Test.Hspec
import List
import Control.Exception (evaluate)

specs :: SpecWith ()
specs = describe "List" $ do
  describe "Empty" $ do
    it "returns empty list" $ do
      empty `shouldBe` (Nil :: List Int)

  describe "isEmpty" $ do
    it "returns true for empty list" $ do
      isEmpty Nil `shouldBe` True
    it "returns false for non-empty list" $ do
      isEmpty (Cons 1 Nil) `shouldBe` False

  describe "cons" $ do
    it "appends element on empty list" $ do
      cons 1 Nil `shouldBe` (Cons 1 Nil)
    it "appends element on non-empty list" $ do
      cons 1 (Cons 2 Nil) `shouldBe` (Cons 1 (Cons 2 Nil))

  describe "head" $ do
    it "works for non-empty list" $ do
      hd (Cons 2 (Cons 3 Nil)) `shouldBe` 2
    it "raises exception for empty list" $ do
      evaluate (hd Nil) `shouldThrow` anyException

  describe "tail" $ do
    it "works for non-empty list" $ do
      tl (Cons 2 (Cons 3 Nil)) `shouldBe` Cons 3 Nil
    it "raises exception for empty list" $ do
      evaluate (tl Nil) `shouldThrow` anyException

  -- Exercises
  describe "append" $ do
    it "appends empty lists" $ do
      append Nil Nil `shouldBe` (Nil :: List Int)
    it "appends non-empty lists" $ do
      append Nil (Cons 2 Nil) `shouldBe` Cons 2 Nil
      append (Cons 3 Nil) Nil `shouldBe` Cons 3 Nil
      append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil))  `shouldBe` Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

  describe "update" $ do
    it "raises exception for empty list" $ do
      evaluate (update Nil 2 1) `shouldThrow` anyException
    it "raises exception index greater than length" $ do
      evaluate (update (Cons 1 Nil) 2 10) `shouldThrow` anyException
    it "returns updated list" $ do
      update (Cons 1 (Cons 2 Nil)) 2 0 `shouldBe` Cons 2 (Cons 2 Nil)
