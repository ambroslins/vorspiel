-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- Yet another alternative Prelude.
module Vorspiel
  ( -- * Basic types and functions

    -- ** Bool
    Prelude.Bool (..),
    Prelude.not,
    (Prelude.&&),
    (Prelude.||),
    Prelude.otherwise,

    -- ** Maybe
    Prelude.Maybe (..),
    Prelude.maybe,

    -- ** Either
    Prelude.Either (..),
    Prelude.either,

    -- ** Ordering
    Prelude.Ordering (..),
    Ord.comparing,

    -- ** Char
    Prelude.Char,

    -- ** NonEmpty
    NonEmpty (..),
    Foldable.nonEmpty,
    Foldable.withNonEmpty,
    NonEmpty.head,
    NonEmpty.tail,
    NonEmpty.last,
    NonEmpty.init,

    -- ** String
    Prelude.String,

    -- ** Numbers
    Data.Int.Int,
    Data.Int.Int8,
    Data.Int.Int16,
    Data.Int.Int32,
    Data.Int.Int64,
    Prelude.Integer,
    Data.Word.Word,
    Data.Word.Word8,
    Data.Word.Word16,
    Data.Word.Word32,
    Data.Word.Word64,
    Natural,
    Prelude.Rational,
    Prelude.Float,
    Prelude.Double,

    -- ** Text
    Text.Text,
    Text.replace,

    -- *** Case conversion
    Text.toLower,
    Text.toUpper,
    Text.toTitle,

    -- *** Justification
    Text.justifyLeft,
    Text.justifyRight,
    Text.center,

    -- *** Strip
    Text.strip,
    Text.stripStart,
    Text.stripEnd,

    -- *** Lines and words
    Text.lines,
    Text.words,
    Text.unlines,
    Text.unwords,

    -- *** Predicates
    Text.isPrefixOf,
    Text.isSuffixOf,
    Text.isInfixOf,

    -- *** View patterns
    Text.stripPrefix,
    Text.stripSuffix,
    Text.commonPrefixes,

    -- ** Sets
    Set,
    IntSet,
    HashSet,

    -- ** Maps
    Map,
    IntMap,
    HashMap,

    -- ** Sequence
    Sequence.Seq (..),
    (Sequence.<|),
    (Sequence.|>),

    -- ** Void
    Void,
    absurd,

    -- * Basic classes
    Prelude.Eq (..),
    Prelude.Ord (..),
    Hashable.Hashable (..),
    Ord.Down (..),
    Prelude.Enum,
    Prelude.Bounded,
    Prelude.Show (show),
    Prelude.Read,
    read,

    -- ** Semigroup
    Prelude.Semigroup (..),
    Semigroup.All (..),
    Semigroup.Any (..),
    Semigroup.Min (..),
    Semigroup.Max (..),
    Semigroup.Sum (..),
    Semigroup.Product (..),

    -- ** Monoid
    Prelude.Monoid (..),

    -- ** Functor
    Functor.Functor (..),
    (Functor.<$>),
    (Functor.$>),
    (Functor.<&>),
    Functor.void,

    -- *** Contravariant
    Contravariant.Contravariant (..),
    (Contravariant.>$<),
    (Contravariant.$<),
    Contravariant.Predicate (..),
    Contravariant.Comparison (..),
    Contravariant.Equivalence (..),

    -- ** Bifunctor
    Bifunctor.Bifunctor (..),

    -- ** Foldable
    Prelude.Foldable
      ( fold,
        foldMap,
        foldMap',
        foldl,
        foldl',
        foldr,
        foldr',
        length,
        toList
      ),
    Foldable.and,
    Foldable.or,
    Foldable.all,
    Foldable.any,
    Foldable.foldrM,
    Foldable.foldlM,
    Foldable.sum,
    Foldable.product,

    -- *** Foldable1
    Foldable.Foldable1 (..),
    Foldable.foldr1,
    Foldable.foldl1,
    Foldable.minimum,
    Foldable.maximum,
    Foldable.minimumBy,
    Foldable.maximumBy,

    -- ** Traversable
    Prelude.Traversable (..),

    -- ** Applicative
    Prelude.Applicative (..),
    pass,
    Monad.when,
    Monad.unless,
    Applicative.liftA,
    Applicative.liftA3,
    forever,
    Applicative.Const (..),
    Applicative.ZipList (..),

    -- ** Alternative
    Applicative.Alternative (empty, (<|>)),
    Applicative.many,
    some,
    Applicative.optional,
    Foldable.asum,
    Monad.guard,

    -- ** Monad
    Prelude.Monad (..),
    Monad.join,
    (Monad.=<<),
    (Monad.>=>),
    (Monad.<=<),
    Monad.mapM_,
    Monad.forM,
    Monad.forM_,
    Monad.sequence_,
    Monad.zipWithM,
    Monad.zipWithM_,
    Monad.foldM,
    Monad.foldM_,
    Monad.replicateM,
    Monad.replicateM_,

    -- ** Numeric
    Prelude.Num (..),
    Prelude.Real (..),
    Prelude.Integral (..),
    Prelude.Fractional (..),
    Prelude.Floating (..),
    Prelude.subtract,
    Prelude.even,
    Prelude.odd,
    Prelude.gcd,
    Prelude.lcm,
    (Prelude.^),
    (Prelude.^^),
    Prelude.fromIntegral,
    Prelude.realToFrac,

    -- * Miscellaneous functions
    identity,
    Prelude.const,
    (Prelude..),
    Prelude.flip,
    (Prelude.$),
    Prelude.asTypeOf,
    Prelude.error,
    Prelude.undefined,
    Prelude.seq,
    (Prelude.$!),
    Function.on,
    (Function.&),
    Function.fix,

    -- * IO
    Prelude.IO,
    Text.IO.readFile,
    Text.IO.writeFile,
    Text.IO.appendFile,
    Text.IO.interact,
    Text.IO.getContents,
    Text.IO.getLine,
    Text.IO.putStr,
    Text.IO.putStrLn,
    Prelude.print,

    -- * Exceptions
    Exception.Exception (..),
    Exception.SomeException (..),
  )
where

import Control.Applicative qualified as Applicative
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Bifunctor qualified as Bifunctor
import Data.Function qualified as Function
import Data.Functor qualified as Functor
import Data.Functor.Contravariant qualified as Contravariant
import Data.Hashable qualified as Hashable
import Data.Int qualified
import Data.Ord qualified as Ord
import Data.Semigroup qualified as Semigroup
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Void (absurd)
import Data.Word qualified
import HashMap (HashMap)
import HashSet (HashSet)
import IntMap (IntMap)
import IntSet (IntSet)
import Map (Map)
import NonEmpty qualified
import Sequence qualified
import Set (Set)
import Text.Read (Read, readMaybe)
import Vorspiel.Foldable qualified as Foldable
import Vorspiel.Prelude
import Prelude qualified

-- | Identity function.
--
-- @
-- identity x = x
-- @
identity :: a -> a
identity x = x

-- | Parse a string using the Read instance. Succeeds if there is exactly one valid result. Alias for 'readMaybe'.
read :: Read a => String -> Maybe a
read = readMaybe

-- | One or more
some :: Applicative.Alternative f => f a -> f (NonEmpty a)
some v = (:|) <$> v <*> Applicative.many v

-- | Alias for @pure ()@.
-- >>> pass :: Maybe ()
-- Just ()
pass :: Applicative.Applicative f => f ()
pass = Applicative.pure ()

-- | Repeat an action indefinitely.
forever :: Applicative.Applicative f => f a -> f Void
forever = Monad.forever