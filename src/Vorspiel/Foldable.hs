module Vorspiel.Foldable
  ( module Data.Foldable,
    module Data.Semigroup.Foldable,
    module Vorspiel.Foldable,
    module Data.List.NonEmpty,
  )
where

import Data.Foldable
  ( Foldable
      ( fold,
        foldMap,
        foldMap',
        foldl',
        foldr,
        length,
        toList
      ),
    all,
    and,
    any,
    asum,
    foldlM,
    foldrM,
    or,
  )
import Data.Foldable qualified
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, maybe)
import Data.Semigroup
  ( Product (..),
    Sum (..),
  )
import Data.Semigroup.Foldable (Foldable1 (..))
import Prelude
  ( Maybe (..),
    Num (..),
    Ord (..),
    Ordering,
    error,
    fmap,
    ($!),
    (.),
  )

-- $setup
-- >>> import Prelude (mod)
-- >>> import Data.Ord (compare)

-- | A variant of 'foldr' for non-empty structures, and thus does not require a base case.
foldr1 :: Foldable1 f => (a -> a -> a) -> f a -> a
foldr1 = Data.Foldable.foldr1

-- | A variant of 'fold'' for non-empty structures, and thus does not require a base case.
foldl1' :: Foldable1 f => (a -> a -> a) -> f a -> a
foldl1' f = fromMaybe (error "foldl1") . Data.Foldable.foldl' mf Nothing
  where
    mf m y = Just $! maybe y (`f` y) m

-- | Build a 'NonEmpty' list from some structure,  or return `Nothing` if the structure is empty.
--
-- >>> nonEmpty [1, 2, 3]
-- Just (1 :| [2,3])
--
-- >>> nonEmpty []
-- Nothing
nonEmpty :: Foldable f => f a -> Maybe (NonEmpty a)
nonEmpty = NonEmpty.nonEmpty . toList

-- | Apply a function expecting a 'NonEmpty' list to some structure, or return `Nothing` if the structure is empty.
--
-- >>> withNonEmpty maximum [1, 2, 3]
-- Just 3
--
-- >>> withNonEmpty maximum []
-- Nothing
withNonEmpty :: Foldable f => (NonEmpty a -> b) -> f a -> Maybe b
withNonEmpty f = fmap f . nonEmpty . toList

-- | Compute the sum of all elements of a structure.
--
-- >>> sum [1, 2, 3, 4]
-- 10
--
-- >>> sum []
-- 0
sum :: (Foldable f, Num a) => f a -> a
sum = getSum . foldMap' Sum

-- | Compute the product of all elements of a structure.
--
-- >>> product [1, 2, 3, 4]
-- 24
--
-- >>> product []
-- 1
product :: (Foldable f, Num a) => f a -> a
product = getProduct . foldMap' Product

-- | The largest element of a non-empty structure.
--
-- >>> maximum (1 :| [2, 3])
-- 3
maximum :: (Foldable1 f, Ord a) => f a -> a
maximum = Data.Foldable.maximum

-- | The least element of a non-empty structure.
--
-- >>> minimum (1 :| [2, 3])
-- 1
minimum :: (Foldable1 f, Ord a) => f a -> a
minimum = Data.Foldable.minimum

-- | The largest element of a non-empty structure with respect to the given comparison function.
--
-- >>> maximumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) (1 :| [2, 3, 4, 5, 6])
-- 5
maximumBy :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
maximumBy = Data.Foldable.maximumBy

-- | The least element of a non-empty structure with respect to the given comparison function.
--
-- >>> minimumBy (\x y -> compare (x `mod` 3) (y `mod` 3)) (1 :| [2, 3, 4, 5, 6])
-- 3
minimumBy :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
minimumBy = Data.Foldable.minimumBy
