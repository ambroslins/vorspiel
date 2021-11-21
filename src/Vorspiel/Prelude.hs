module Vorspiel.Prelude
  ( module Prelude,
    module Numeric.Natural,
    module Data.Foldable,
    module Data.Function,
    module Data.Monoid,
    module Data.List.NonEmpty,
    module Data.Void,
    module Data.Ord,
    unsafeNonEmpty,
  )
where

import Data.Foldable (Foldable (foldMap, foldMap', foldl', foldr, toList))
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Product (..), Sum (..))
import Data.Ord (comparing)
import Data.Void (Void)
import GHC.List (errorEmptyList)
import Numeric.Natural (Natural)
import Prelude
  ( Applicative (..),
    Bool (..),
    Bounded (..),
    Char,
    Double,
    Either (..),
    Enum,
    Eq (..),
    Float,
    Floating (..),
    Fractional (..),
    Functor (..),
    IO,
    Int,
    Integer,
    Integral (..),
    Maybe (..),
    Monad (..),
    Monoid (mempty),
    Num (..),
    Ord (..),
    Ordering (..),
    Rational,
    Real (..),
    RealFloat (..),
    RealFrac (..),
    Semigroup (..),
    Show (..),
    String,
    Word,
    const,
    curry,
    either,
    even,
    flip,
    fromIntegral,
    fst,
    gcd,
    id,
    lcm,
    maybe,
    not,
    odd,
    otherwise,
    realToFrac,
    seq,
    snd,
    subtract,
    uncurry,
    ($),
    ($!),
    (&&),
    (++),
    (.),
    (<$>),
    (^),
    (^^),
    (||),
  )

unsafeNonEmpty :: [a] -> NonEmpty a
unsafeNonEmpty = \case
  [] -> errorEmptyList "unsafeNonEmpty"
  (x : xs) -> x :| xs
