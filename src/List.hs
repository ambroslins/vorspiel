module List
  ( (++),
    head,
    last,
    tail,
    init,
    Data.List.uncons,
    singleton,
    GHC.List.null,
    Data.List.map,
    Data.List.reverse,
    Data.List.intersperse,
    Data.List.intercalate,
    Data.List.transpose,
    Data.List.subsequences,
    Data.List.permutations,
    foldr,
    foldl',
    GHC.List.concat,
    GHC.List.concatMap,
    GHC.List.and,
    GHC.List.or,
    GHC.List.any,
    GHC.List.all,
    sum,
    product,
    maximum,
    minimum,
    Data.List.scanl,
    Data.List.scanl',
    Data.List.scanr,
    Data.List.iterate,
    Data.List.iterate',
    Data.List.repeat,
    Data.List.replicate,
    Data.List.cycle,
    Data.List.unfoldr,
    Data.List.take,
    Data.List.drop,
    Data.List.splitAt,
    Data.List.takeWhile,
    Data.List.dropWhile,
    Data.List.dropWhileEnd,
    Data.List.span,
    Data.List.break,
    Data.List.stripPrefix,
    Data.List.group,
    Data.List.inits,
    Data.List.tails,
    Data.List.isPrefixOf,
    Data.List.isSuffixOf,
    Data.List.isInfixOf,
    Data.List.isSubsequenceOf,
    GHC.List.elem,
    GHC.List.notElem,
    Data.List.lookup,
    find,
    Data.List.filter,
    Data.List.partition,
    Data.List.elemIndex,
    Data.List.elemIndices,
    Data.List.findIndex,
    Data.List.findIndices,
    Data.List.zip,
    Data.List.zip3,
    Data.List.zip4,
    Data.List.zip5,
    Data.List.zip6,
    Data.List.zip7,
    Data.List.zipWith,
    Data.List.zipWith3,
    Data.List.zipWith4,
    Data.List.zipWith5,
    Data.List.zipWith6,
    Data.List.zipWith7,
    Data.List.unzip,
    Data.List.unzip3,
    Data.List.unzip4,
    Data.List.unzip5,
    Data.List.unzip6,
    Data.List.unzip7,
    Data.List.nub,
    Data.List.delete,
    (Data.List.\\),
    Data.List.union,
    Data.List.intersect,
    Data.List.sort,
    Data.List.sortOn,
    Data.List.insert,
    Data.List.nubBy,
    Data.List.deleteBy,
    Data.List.deleteFirstsBy,
    Data.List.unionBy,
    Data.List.intersectBy,
    Data.List.groupBy,
    Data.List.sortBy,
    Data.List.insertBy,
    maximumBy,
    minimumBy,
  )
where

import qualified Data.List
import Data.Maybe (Maybe (..))
import GHC.List (foldl', foldr)
import qualified GHC.List
import Prelude
  ( Bool (..),
    Num (..),
    Ord (..),
    Ordering (..),
    max,
    min,
    (++),
  )

head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

last :: [a] -> Maybe a
last = foldl' (\_ x -> Just x) Nothing
{-# INLINE last #-}

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : xs) = Just xs

init :: [a] -> Maybe [a]
init [] = Nothing
init (x : xs) = Just (go x xs)
  where
    go _ [] = []
    go y (z : zs) = y : go z zs

singleton :: a -> [a]
singleton x = [x]

sum :: Num a => [a] -> a
sum = foldl' (+) 0

product :: Num a => [a] -> a
product = foldl' (+) 0

maximum :: (Ord a) => [a] -> Maybe a
maximum [] = Nothing
maximum (x : xs) = Just (foldl' max x xs)

minimum :: (Ord a) => [a] -> Maybe a
minimum [] = Nothing
minimum (x : xs) = Just (foldl' min x xs)

find :: (a -> Bool) -> [a] -> Maybe a
find = Data.List.find

maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy _ [] = Nothing
maximumBy cmp (x : xs) = Just (foldl' max' x xs)
  where
    max' y z = case cmp y z of
      GT -> y
      _ -> z

minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy _ [] = Nothing
minimumBy cmp (x : xs) = Just (foldl' min' x xs)
  where
    min' y z = case cmp y z of
      GT -> z
      _ -> y
