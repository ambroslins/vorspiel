module NonEmptySpec where

import List qualified
import NonEmpty qualified
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Vorspiel

spec :: Spec
spec = do
  let toList2 = List.map toList . toList
  describe "concat" $ do
    prop "is equivalent to List.concat" $
      \xss -> toList (NonEmpty.concat xss) === List.concat (toList2 xss :: [[Int]])

  describe "concatMap" $ do
    prop "is equivalent to List.concatMap" $
      \xs f ->
        toList (NonEmpty.concatMap (applyFun f) xs)
          === List.concatMap (toList . applyFun f :: Int -> [Int]) (toList xs :: [Int])

  describe "intercalate" $ do
    prop "is equivalent to List.intercalate" $
      \xs xss -> toList (NonEmpty.intercalate xs xss) === List.intercalate (xs :: [Int]) (toList2 xss)

  describe "subsequences" $ do
    modifyMaxSize (const 8) $
      prop "is equivalent to List.subsequences" $
        \xs -> [] :| toList2 (NonEmpty.subsequences xs) === List.subsequences (toList xs :: [Int])

  describe "permutations" $ do
    modifyMaxSize (const 8) $
      prop "is equivalent to List.permutations" $
        \xs -> NonEmpty.map toList (NonEmpty.permutations xs) === List.permutations (toList xs :: [Int])

  describe "sortOn" $ do
    prop "is equivalent to List.sortOn" $
      \xs f -> toList (NonEmpty.sortOn (applyFun f) xs) === List.sortOn (applyFun f :: Int -> Int) (toList xs :: [Int])