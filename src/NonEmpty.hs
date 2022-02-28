-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- Functions to work with 'NonEmpty' lists.
module NonEmpty
  ( NonEmpty (..),

    -- ** Length information
    length,

    -- ** Indexing
    Base.head,
    Base.last,

    -- ** Sublists
    Base.init,
    Base.tail,
    Base.take,
    Base.drop,
    Base.splitAt,
    Base.uncons,

    -- * Transformations
    map,
    Base.reverse,
    Base.inits,
    Base.tails,
    Base.intersperse,
    intercalate,
    Base.transpose,
    subsequences,
    permutations,

    -- * Construction
    singleton,
    (Base.<|),
    (|>),

    -- ** Unfold
    Base.unfoldr,

    -- ** Concatenation
    Base.cons,
    snoc,
    (++),
    concat,
    concatMap,

    -- ** Scans
    Base.scanr,
    Base.scanl,
    Base.scanr1,
    Base.scanl1,

    -- * Working with predicates

    -- ** Filter
    Base.filter,

    -- ** Partition
    Base.partition,
    Base.span,
    Base.break,
    Base.takeWhile,
    Base.dropWhile,
    Base.group,
    Base.groupBy,

    -- * Zipping
    Base.zip,
    Base.zipWith,

    -- * Unzipping
    Base.unzip,

    -- * Sorted lists
    Base.sort,
    sortOn,
    Base.sortBy,
    Base.insert,
  )
where

import Data.List.NonEmpty (cons, length, map, nonEmpty)
import Data.List.NonEmpty qualified as Base
import List qualified
import Vorspiel.Foldable (foldMap1, foldr, foldr1)
import Vorspiel.Prelude hiding (foldMap, foldMap', foldl', foldr)

-- $setup
-- >>> import Vorspiel.Foldable (toList, sum)
-- >>> import Prelude (Int)
-- >>> import Test.QuickCheck.Instances
-- >>> identity x = x

-- | Make a 'NonEmpty' list with a single element.
singleton :: a -> NonEmpty a
singleton x = x :| []

-- | Append an element to the stream.
(|>) :: [a] -> a -> NonEmpty a
xs |> x = foldr Base.cons (singleton x) xs

-- | Append an element to the end of a 'NonEmpty' list.
snoc :: NonEmpty a -> a -> NonEmpty a
snoc xs x = foldr Base.cons (singleton x) xs

-- | Concatenate a 'NonEmpty' list of 'NonEmpty' lists.
--
-- prop> length (concat xs) == sum (map length (xs :: NonEmpty (NonEmpty Int)))
concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat = foldr1 (<>)

-- | Map a function over a 'NonEmpty' list and concatenate the results.
--
-- prop> concatMap identity xs == concat (xs :: NonEmpty (NonEmpty Int))
concatMap :: (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap = foldMap1

-- | @'intercalate' xs xss@ inserts the list @xs@ in between the 'NonEmpty' lists in @xss@ and concatenates the result.
intercalate :: [a] -> NonEmpty (NonEmpty a) -> NonEmpty a
intercalate = maybe concat (\x -> concat . Base.intersperse x) . nonEmpty

-- | @'subsequences' xs@ returns a 'NonEmpty' list of all 'NonEmpty' subsequences of @xs@.
--
-- >>> toList <$> subsequences ('a' :| "bc")
-- "a" :| ["b","ab","c","ac","bc","abc"]
subsequences :: NonEmpty a -> NonEmpty (NonEmpty a)
subsequences (x :| xs) = case nonEmpty xs of
  Nothing -> singleton sx
  Just xs' -> sx :| foldr f [] (subsequences xs')
  where
    sx = singleton x
    f ys r = ys : cons x ys : r

-- | @'permutations' xs@ returns a 'NonEmpty' list of all permutations of @xs@.
--
-- >>> toList <$> permutations ('a' :| "bc")
-- "abc" :| ["bac","cba","bca","cab","acb"]
permutations :: NonEmpty a -> NonEmpty (NonEmpty a)
permutations = map unsafeNonEmpty . List.permutations . toList

-- | Sort a list by comparing the results of a key function applied to each element.
-- @sortOn f@ is equivalent to @sortBy (comparing f)@,
-- but has the performance advantage of only evaluating @f@ once for each element in the input list.
--
-- Elements are arranged from lowest to highest, keeping duplicates in the order they appeared in the input.
sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = map snd . Base.sortBy (comparing fst) . map (\x -> let !y = f x in (y, x))