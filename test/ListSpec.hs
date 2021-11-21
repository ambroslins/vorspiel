module ListSpec where

import List qualified
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Vorspiel

spec :: Spec
spec = do
  describe "head" $ do
    it "returns the first element" $ List.head [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
    it "returns Nothing if the list is empty" $ List.head [] `shouldBe` (Nothing :: Maybe Int)

  describe "last" $ do
    it "returns the last element" $ List.last [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
    it "returns Nothing if the list is empty" $ List.last [] `shouldBe` (Nothing :: Maybe Int)

  describe "tail" $ do
    it "returns the elements after the head" $ List.tail [1, 2, 3] `shouldBe` (Just [2, 3] :: Maybe [Int])
    it "returns Nothing if the list is empty" $ List.tail [] `shouldBe` (Nothing :: Maybe [Int])

  describe "init" $ do
    it "returns the elements before the last" $ List.init [1, 2, 3] `shouldBe` (Just [1, 2] :: Maybe [Int])
    it "returns Nothing if the list is empty" $ List.init [] `shouldBe` (Nothing :: Maybe [Int])

  describe "maximum" $ do
    it "returns the largest elements" $ List.maximum [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
    it "returns Nothing if the list is empty" $ List.maximum [] `shouldBe` (Nothing :: Maybe Int)
    prop "maximum is greater or equal to all elements" $
      \xs -> maybe discard (\x -> all (x >=) xs) $ List.maximum (xs :: [Int])

  describe "minimum" $ do
    it "returns the least elements" $ List.minimum [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
    it "returns Nothing if the list is empty" $ List.minimum [] `shouldBe` (Nothing :: Maybe Int)
    prop "minimum is less than or equal to all elements" $
      \xs -> maybe discard (\x -> all (x <=) xs) $ List.minimum (xs :: [Int])

  describe "maximumBy" $ do
    prop "'maximumBy compare' is equal to maximum" $
      \xs -> List.maximumBy compare xs === List.maximum (xs :: [Int])

  describe "minimumBy" $ do
    prop "'minimumBy compare' is equal to minimum" $
      \xs -> List.minimumBy compare xs === List.minimum (xs :: [Int])

  describe "subsequences" $
    modifyMaxSize (const 8) $ do
      prop "all elements are true subsequences of the input" $
        \xs -> all (`List.isSubsequenceOf` xs) $ List.subsequences (xs :: [Int])
      prop "there are 2^length subsequences" $
        \xs -> length (List.subsequences xs) === 2 ^ length (xs :: [Int])

  describe "permutations" $
    modifyMaxSize (const 8) $ do
      prop "all permutations are equal if sorted" $
        \xs -> let sorted = List.sort xs in all (\ys -> List.sort ys == sorted) $ List.permutations (xs :: [Int])
      let factorial n = product [1 .. n]
      prop "there are factorial(length) permutations" $
        \xs -> factorial (length xs) === length (List.permutations (xs :: [Int]))
