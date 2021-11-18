-- |
-- Module      : Vorspiel
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

    -- ** Char
    Prelude.Char,

    -- ** NonEmpty
    NonEmpty (..),
    NonEmpty.head,
    NonEmpty.tail,
    NonEmpty.last,
    NonEmpty.init,

    -- ** String
    Prelude.String,

    -- ** Numbers
    Prelude.Int,
    Prelude.Integer,
    Prelude.Word,
    Natural,
    Prelude.Rational,
    Prelude.Float,
    Prelude.Double,

    -- ** Text
    Text.Text,

    -- * Void
    Void,
    absurd,

    -- * Basic classes
    Prelude.Eq (..),
    Prelude.Ord (..),
    Prelude.Enum,
    Prelude.Bounded,
    Prelude.Show (show),
    Prelude.Read,
    read,

    -- ** Semigroup
    Prelude.Semigroup (..),

    -- ** Monoid
    Prelude.Monoid (..),

    -- ** Functor
    Prelude.Functor (..),
    (Prelude.<$>),

    -- ** Applicative
    Prelude.Applicative (..),

    -- ** Alternative
    Applicative.Alternative (empty, (<|>)),
    Applicative.many,
    some,

    -- ** Monad
    Prelude.Monad (..),
    (Prelude.=<<),
    Prelude.mapM_,
    Prelude.sequence_,

    -- ** Foldable
    Prelude.Foldable (foldr, foldMap),
    Foldable.foldMap',

    -- ** Traversable
    Prelude.Traversable (..),

    -- * Miscellaneous functions
    Prelude.id,
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
  )
where

import Control.Applicative qualified as Applicative
import Data.Foldable qualified as Foldable
import Data.Function qualified as Function
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Void (Void, absurd)
import Text.Read (Read, readMaybe)
import Vorspiel.Prelude
import Prelude qualified

-- | Parse a string using the Read instance. Succeeds if there is exactly one valid result
read :: Read a => String -> Maybe a
read = readMaybe

-- | One or more
some :: Applicative.Alternative f => f a -> f (NonEmpty a)
some v = (:|) <$> v <*> Applicative.many v