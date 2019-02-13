module ListSpec where

import Test.Hspec
import List

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
      hd (Cons 2 (Cons 3 Nil)) `shouldBe` Just 2
    it "returns Nothing for empty list" $ do
      hd Nil `shouldBe` (Nothing :: Maybe (List Int))

  describe "tail" $ do
    it "works for non-empty list" $ do
      tl (Cons 2 (Cons 3 Nil)) `shouldBe` Just (Cons 3 Nil)
    it "returns Nothing for empty list" $ do
      tl Nil `shouldBe` (Nothing :: Maybe (List Int))
