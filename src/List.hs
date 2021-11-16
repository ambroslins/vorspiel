module List
  ( -- * Accessors

    -- ** Length information
    GHC.List.length,
    GHC.List.null,

    -- ** Indexing
    head,
    last,

    -- ** Sublists
    init,
    tail,
    Data.List.take,
    Data.List.drop,
    Data.List.splitAt,
    Data.List.stripPrefix,
    Data.List.uncons,

    -- * Transformations
    Data.List.map,
    Data.List.reverse,
    Data.List.intersperse,
    Data.List.intercalate,
    Data.List.transpose,
    Data.List.subsequences,
    Data.List.permutations,
    Data.List.inits,
    Data.List.tails,

    -- * Reductions (folds)
    foldr,
    foldl',

    -- ** Aggregate functions
    GHC.List.and,
    GHC.List.or,
    GHC.List.any,
    GHC.List.all,
    sum,
    product,
    maximum,
    minimum,
    maximumBy,
    minimumBy,

    -- * Construction
    empty,
    singleton,
    Data.List.replicate,

    -- ** Unfold
    Data.List.unfoldr,

    -- ** Concatenation
    (++),
    GHC.List.concat,
    GHC.List.concatMap,

    -- ** Scans
    Data.List.scanr,
    Data.List.scanl,
    Data.List.scanl',

    -- * Working with predicates

    -- ** Filter
    Data.List.filter,
    Data.List.delete,
    Data.List.deleteBy,

    -- ** Partition
    Data.List.partition,
    Data.List.span,
    Data.List.break,
    Data.List.takeWhile,
    Data.List.dropWhile,
    Data.List.dropWhileEnd,
    Data.List.group,
    Data.List.groupBy,

    -- * Searching
    GHC.List.elem,
    GHC.List.notElem,
    Data.List.lookup,
    find,

    -- ** Index search
    Data.List.elemIndex,
    Data.List.elemIndices,
    Data.List.findIndex,
    Data.List.findIndices,

    -- * Predicates
    Data.List.isPrefixOf,
    Data.List.isSuffixOf,
    Data.List.isInfixOf,
    Data.List.isSubsequenceOf,

    -- * Zipping
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

    -- * Unzipping
    Data.List.unzip,
    Data.List.unzip3,
    Data.List.unzip4,
    Data.List.unzip5,
    Data.List.unzip6,
    Data.List.unzip7,

    -- * Sorted lists
    Data.List.sort,
    Data.List.sortOn,
    Data.List.sortBy,
    Data.List.insert,
    Data.List.insertBy,
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

-- $setup
-- >>> import Prelude (even, mod)

-- | \(\mathcal{O}(1)\). Empty list.
empty :: [a]
empty = []

-- | \(\mathcal{O}(1)\). Make a singleton list.
--
-- >>> singleton 1
-- [1]
singleton :: a -> [a]
singleton x = [x]

-- | \(\mathcal{O}(1)\). The first element of a list, or `Nothing` if empty.
--
-- >>> head [1, 2, 3]
-- Just 1
-- >>> head []
-- Nothing
head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

-- | \(\mathcal{O}(n)\). The last element of a list, or `Nothing` if empty.
--
-- >>> last [1, 2, 3]
-- Just 3
-- >>> last []
-- Nothing
last :: [a] -> Maybe a
last = foldl' (\_ x -> Just x) Nothing
{-# INLINE last #-}

-- | \(\mathcal{O}(1)\). The elements after the head of a list, or `Nothing` if empty.
--
-- >>> tail [1, 2, 3]
-- Just [2,3]
-- >>> tail [1]
-- Just []
-- >>> tail []
-- Nothing
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : xs) = Just xs

-- | \(\mathcal{O}(n)\). All the elements of a list except the last one, or `Nothing` if empty.
--
-- >>> init [1, 2, 3]
-- Just [1,2]
-- >>> init [1]
-- Just []
-- >>> init []
-- Nothing
init :: [a] -> Maybe [a]
init [] = Nothing
init (x : xs) = Just (go x xs)
  where
    go _ [] = []
    go y (z : zs) = y : go z zs

-- | \(\mathcal{O}(n)\). Compute the sum of all elements in the list
--
-- >>> sum [1, 2, 3, 4]
-- 10
--
-- >>> sum []
-- 0
sum :: Num a => [a] -> a
sum = foldl' (+) 0

-- | \(\mathcal{O}(n)\). Compute the product of all elements in the list
--
-- >>> product [1, 2, 3, 4]
-- 24
--
-- >>> product []
-- 1
product :: Num a => [a] -> a
product = foldl' (*) 1

withCons :: (a -> [a] -> b) -> [a] -> Maybe b
withCons f = \case
  [] -> Nothing
  (x : xs) -> Just (f x xs)

-- | \(\mathcal{O}(1)\). The largest element of a list, or `Nothing` if empty.
--
-- >>> maximum [1, 2, 3]
-- Just 3
maximum :: (Ord a) => [a] -> Maybe a
maximum = withCons (foldl' max)

-- | \(\mathcal{O}(1)\). The least element of a list, or `Nothing` if empty.
--
-- >>> minimum [1, 2, 3]
-- Just 1
minimum :: (Ord a) => [a] -> Maybe a
minimum = withCons (foldl' min)

-- | \(\mathcal{O}(1)\). The first element of a list matching the predicate, or `Nothing` if there is no such element.
--
-- >>> find even [1, 2, 3]
-- Just 2
find :: (a -> Bool) -> [a] -> Maybe a
find = Data.List.find

-- | \(\mathcal{O}(1)\). The largest element of a list with respect to the given comparison function, or `Nothing` if empty.
--
-- >>> maximumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) [1, 2, 3, 4, 5, 6]
-- Just 5
maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy cmp = withCons (foldl' max')
  where
    max' y z = case cmp y z of
      GT -> y
      _ -> z

-- | \(\mathcal{O}(1)\). The least element of a list with respect to the given comparison function, or `Nothing` if empty.
--
-- >>> minimumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) [1, 2, 3, 4, 5, 6]
-- Just 3
minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy cmp = withCons (foldl' min')
  where
    min' y z = case cmp y z of
      GT -> z
      _ -> y
