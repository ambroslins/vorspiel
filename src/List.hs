-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- Functions to work with lists. Mostly reexports from "Data.List".
module List
  ( -- * Accessors

    -- ** Length information
    GHC.List.length,
    GHC.List.null,

    -- ** Indexing
    index,
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
    unsnoc,

    -- * Construction
    empty,
    singleton,
    Data.List.replicate,
    generate,

    -- ** Concatenation
    cons,
    snoc,
    (++),
    GHC.List.concat,
    GHC.List.concatMap,

    -- ** Unfold
    Data.List.unfoldr,

    -- * Modifying Lists

    -- ** Index Update
    set,
    update,
    alter,

    -- ** Map
    Data.List.map,
    imap,

    -- ** Intersperse
    Data.List.intersperse,
    Data.List.intercalate,

    -- * Permuations and Subsequences
    Data.List.reverse,
    Data.List.inits,
    Data.List.tails,
    Data.List.transpose,
    subsequences,
    permutations,

    -- * Folds
    GHC.List.foldr,
    GHC.List.foldl',

    -- ** Minimum and Maximum
    maximum,
    minimum,
    maximumBy,
    minimumBy,

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

    -- * Ordering
    compareLength,
    comparingLength,

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

import Data.Bifunctor (first)
import Data.List qualified
import Data.List.NonEmpty qualified as NonEmpty
import GHC.List qualified
import Vorspiel.Foldable (withNonEmpty)
import Vorspiel.Foldable qualified as Foldable
import Vorspiel.Prelude

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

-- | \(\mathcal{O}(n)\). Construct a list of given length by applying the function to the indicies.
--
-- >>> generate 5 (* 2)
-- [0,2,4,6,8]
generate :: Int -> (Int -> a) -> [a]
generate n f = Data.List.map f [0 .. (n - 1)]

-- | \(\mathcal{O}(n)\). Returns the element the specified index, or `Nothing` if the index is out-of-bounds.
--
-- >>> index ['a', 'b', 'c'] 1
-- Just 'b'
--
-- >>> index ['a', 'b', 'c'] 3
-- Nothing
index :: [a] -> Int -> Maybe a
index xs i = if i < 0 then Nothing else go xs i
  where
    go [] _ = Nothing
    go (y : ys) j = if j == 0 then Just y else go ys (j - 1)

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
last [] = Nothing
last [x] = Just x
last (_ : xs) = last xs

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

-- | \(\mathcal{O}(n)\). Decompose a list into its init and last. Returns `Nothing` if the list is empty.
--
-- >>> unsnoc []
-- Nothing
-- >>> unsnoc [1]
-- Just ([],1)
-- >>> unsnoc [1, 2, 3]
-- Just ([1,2],3)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x : xs) = Just (go x xs)
  where
    go y [] = ([], y)
    go y (z : zs) = first (y :) (go z zs)

-- | \(\mathcal{O}(n)\). Set the element at the given index.
-- Returns the same list if the index is out-of-bounds.
--
-- >>> set 1 4 [1, 2, 3]
-- [1,4,3]
--
-- >>> set 3 4 [1, 2, 3]
-- [1,2,3]
set :: Int -> a -> [a] -> [a]
set i x = update i (const x)

-- | \(\mathcal{O}(n)\). Update the element at the given index.
-- Returns the same list if the index is out-of-bounds.
--
-- >>> update 1 (* 2) [1, 2, 3]
-- [1,4,3]
--
-- >>> update 3 (* 2) [1, 2, 3]
-- [1,2,3]
update :: Int -> (a -> a) -> [a] -> [a]
update i f xs = if i < 0 then xs else go xs i
  where
    go [] _ = []
    go (y : ys) j =
      if j == 0
        then f y : ys
        else y : go ys (j - 1)

-- | \(\mathcal{O}(n)\). Update or delete the element at the given index.
-- Returns the same list if the index is out-of-bounds.
--
-- >>> alter 1 (\x -> if even x then Just (x `div` 2) else Nothing) [1, 2, 3]
-- [1,1,3]
--
-- >>> alter 0 (\x -> if even x then Just (x `div` 2) else Nothing) [1, 2, 3]
-- [2,3]
--
-- >>> alter 3 pure [1, 2, 3]
-- [1,2,3]
alter :: Int -> (a -> Maybe a) -> [a] -> [a]
alter i f xs = if i < 0 then xs else go xs i
  where
    go [] _ = []
    go (y : ys) j =
      if j == 0
        then maybe id (:) (f y) ys
        else y : go ys (j - 1)

-- | \(\mathcal{O}(n)\). Apply a function to every element of a list and its index.
--
-- >>> imap (+) [1, 2, 3]
-- [1,3,5]
imap :: (Int -> a -> a) -> [a] -> [a]
imap f = Data.List.zipWith f [0 ..]

-- | \(\mathcal{O}(n)\). The largest element of a list, or `Nothing` if empty.
--
-- >>> maximum [1, 2, 3]
-- Just 3
maximum :: (Ord a) => [a] -> Maybe a
maximum = withNonEmpty Foldable.maximum

-- | \(\mathcal{O}(n)\). The least element of a list, or `Nothing` if empty.
--
-- >>> minimum [1, 2, 3]
-- Just 1
minimum :: (Ord a) => [a] -> Maybe a
minimum = withNonEmpty Foldable.minimum

-- | \(\mathcal{O}(n)\). The first element of a list matching the predicate, or `Nothing` if there is no such element.
--
-- >>> find even [1, 2, 3]
-- Just 2
find :: (a -> Bool) -> [a] -> Maybe a
find = Data.List.find

-- | \(\mathcal{O}(n)\). The largest element of a list with respect to the given comparison function, or `Nothing` if empty.
--
-- >>> maximumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) [1, 2, 3, 4, 5, 6]
-- Just 5
maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy cmp = withNonEmpty (Foldable.maximumBy cmp)

-- | \(\mathcal{O}(n)\). The least element of a list with respect to the given comparison function, or `Nothing` if empty.
--
-- >>> minimumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) [1, 2, 3, 4, 5, 6]
-- Just 3
minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy cmp = withNonEmpty (Foldable.minimumBy cmp)

-- | \(\mathcal{O}(1)\). Prepend an element to the front of a list. Alias for (:|).
--
-- >>> cons 1 [2, 3]
-- 1 :| [2,3]
cons :: a -> [a] -> NonEmpty a
cons = (:|)

-- | \(\mathcal{O}(n)\). Append an element to the end of a list.
--
-- >>> snoc [1, 2] 3
-- 1 :| [2,3]
snoc :: [a] -> a -> NonEmpty a
snoc xs x = foldr NonEmpty.cons (pure x) xs

-- | \(\mathcal{O}(n)\). Lazily compare the length of list with an `Int`.
--
-- >>> compareLength [1, 2] 3
-- LT
--
-- >>> compareLength [1, 2, 3] 3
-- EQ
--
-- >>> compareLength [0 ..] 3
-- GT
compareLength :: [a] -> Int -> Ordering
compareLength =
  Data.List.foldr
    (\_ acc n -> if n > 0 then acc (n - 1) else GT)
    (compare 0)

-- | \(\mathcal{O}(n)\). Lazily compare the length of two list.
--
-- >>> comparingLength [1, 2] [1, 2, 3]
-- LT
--
-- >>> comparingLength [1, 2, 3] [1, 2, 3]
-- EQ
--
-- >>> comparingLength [0 ..] [1, 2, 3]
-- GT
comparingLength :: [a] -> [a] -> Ordering
comparingLength [] [] = EQ
comparingLength [] (_ : _) = LT
comparingLength (_ : _) [] = GT
comparingLength (_ : xs) (_ : ys) = comparingLength xs ys

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- >>> subsequences "abc"
-- "" :| ["a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> NonEmpty [a]
subsequences = unsafeNonEmpty . Data.List.subsequences

-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- >>> permutations "abc"
-- "abc" :| ["bac","cba","bca","cab","acb"]
permutations :: [a] -> NonEmpty [a]
permutations = unsafeNonEmpty . Data.List.permutations
