module ListSpec where

import List qualified
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function (apply)
import Vorspiel

{- HLINT ignore "Use minimum" -}
{- HLINT ignore "Use maximum" -}

spec :: Spec
spec = do
  describe "generate" $ do
    prop "returns a list with the right length" $
      \(NonNegative n) f -> length (List.generate @Int n (apply f)) === n
    prop "returns indices on idenity" $
      \(NonNegative n) -> List.generate n identity === [0 .. (n - 1)]
    prop "returns same elements as 'imap (f . const)'" $
      \(NonNegative n) f ->
        conjoin $
          List.imap (\i x -> apply f i === x) $
            List.generate @Int n (apply f)

  describe "index" $ do
    it "returns the first element" $
      List.index [1, 2, 3] 0 `shouldBe` Just @Int 1
    it "returns Nothing if the index is out-of-bounds" $
      List.index [] 0 `shouldBe` Nothing @Int
    prop "returned value is element of the list" $
      \(NonNegative i) xs ->
        maybe discard (`List.elem` xs) $
          List.index @Int xs i

  describe "head" $ do
    it "returns the first element" $
      List.head [1, 2, 3] `shouldBe` Just @Int 1
    it "returns Nothing if the list is empty" $
      List.head [] `shouldBe` Nothing @Int

  describe "last" $ do
    it "returns the last element" $
      List.last [1, 2, 3] `shouldBe` Just @Int 3
    it "returns Nothing if the list is empty" $
      List.last [] `shouldBe` Nothing @Int

  describe "tail" $ do
    it "returns the elements after the head" $
      List.tail [1, 2, 3] `shouldBe` Just @[Int] [2, 3]
    it "returns Nothing if the list is empty" $
      List.tail [] `shouldBe` Nothing @[Int]

  describe "init" $ do
    it "returns the elements before the last" $
      List.init [1, 2, 3] `shouldBe` Just @[Int] [1, 2]
    it "returns Nothing if the list is empty" $
      List.init [] `shouldBe` Nothing @[Int]

  describe "unsnoc" $ do
    prop "is equivalent to init and last" $
      \xs -> List.unsnoc @Int xs === liftA2 (,) (List.init xs) (List.last xs)

  describe "set" $ do
    prop "set does change the element at the index" $
      \(NonNegative i) x xs ->
        maybe discard (=== x) $
          List.index @Int (List.set i x xs) i
    prop "does change at most one element" $
      \(NonNegative i) x xs ->
        let n = length xs
         in List.count identity (List.zipWith (==) (List.set @Int i x xs) xs)
              `List.elem` [n, n - 1]

  describe "update" $ do
    prop "set does change the element at the index" $
      \(NonNegative i) f xs ->
        maybe discard ((=== (apply f <$> List.index xs i)) . Just) $
          List.index @Int (List.update i (apply f) xs) i
    prop "does change at most one element" $
      \(NonNegative i) f xs ->
        let n = length xs
         in List.count
              identity
              (List.zipWith (==) (List.update @Int i (apply f) xs) xs)
              `List.elem` [n, n - 1]

  describe "alter" $ do
    prop "set does change the element at the index" $
      \(NonNegative i) f xs ->
        maybe discard ((=== (apply f <$> List.index xs i)) . Just) $
          List.index @Int (List.alter i (Just . apply f) xs) i
    prop "`alter i Just` is `identity` forall i" $
      \(NonNegative i) xs ->
        List.alter @Int i Just xs === xs
    prop "does change at most one element" $
      \(NonNegative i) f xs ->
        let n = length xs
         in length (List.alter @Int i (apply f) xs)
              `List.elem` [n, n - 1]

  describe "maximum" $ do
    it "returns the largest elements" $
      List.maximum [1, 2, 3] `shouldBe` Just @Int 3
    it "returns Nothing if the list is empty" $
      List.maximum [] `shouldBe` Nothing @Int
    prop "maximum is greater or equal to all elements" $
      \xs -> maybe discard (\x -> all (x >=) xs) $ List.maximum @Int xs

  describe "minimum" $ do
    it "returns the least elements" $
      List.minimum [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
    it "returns Nothing if the list is empty" $
      List.minimum [] `shouldBe` (Nothing :: Maybe Int)
    prop "minimum is less than or equal to all elements" $
      \xs -> maybe discard (\x -> all (x <=) xs) $ List.minimum @Int xs

  describe "maximumBy" $ do
    prop "'maximumBy compare' is equal to maximum" $
      \xs -> List.maximumBy compare xs === List.maximum @Int xs

  describe "minimumBy" $ do
    prop "'minimumBy compare' is equal to minimum" $
      \xs -> List.minimumBy compare xs === List.minimum @Int xs

  describe "count" $ do
    prop "is equivalent to `length . filter p" $
      \p xs -> List.count (apply p) xs === length (List.filter @Int (apply p) xs)

  describe "compareLength" $ do
    prop "is equivalent to `compare` on length" $
      \xs n -> List.compareLength @Int xs n === compare (length xs) n
    prop "does terminate on infinite list" $
      \xs n ->
        within 1000000 $
          List.compareLength @Int (getInfiniteList xs) n === GT

  describe "compareLength" $ do
    prop "is equivalent to `compare` on length" $
      \xs ys -> List.comparingLength @Int xs ys === (compare `on` length) xs ys
    prop "does terminate on right infinite list" $
      \xs ys ->
        within 1000000 $
          List.comparingLength @Int xs (getInfiniteList ys) === LT
    prop "does terminate on left infinite list" $
      \xs ys ->
        within 1000000 $
          List.comparingLength @Int (getInfiniteList xs) ys === GT

  describe "subsequences" $
    modifyMaxSize (const 8) $ do
      prop "all elements are true subsequences of the input" $
        \xs -> all (`List.isSubsequenceOf` xs) $ List.subsequences @Int xs
      prop "there are 2^length subsequences" $
        \xs -> length (List.subsequences @Int xs) === 2 ^ length xs

  describe "permutations" $
    modifyMaxSize (const 8) $ do
      prop "all permutations are equal if sorted" $
        \xs ->
          let sorted = List.sort xs
           in conjoin . List.map (\ys -> List.sort ys === sorted) . toList $
                List.permutations @Int xs
      let factorial n = product [1 .. n]
      prop "there are factorial(length) permutations" $
        \xs -> factorial (length xs) === length (List.permutations @Int xs)
