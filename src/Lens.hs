{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- A minimal @lens@ implementation.
module Lens where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Vorspiel.Prelude (Functor, const, (.), (<$>))

-- | The 'Lens' type.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

{- HLINT ignore "Redundant lambda" -}

-- | Build a 'Lens' from a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = \afb s -> sbt s <$> afb (sa s)

-- $setup
-- >>> import Vorspiel
-- >>> sndLens = lens snd (\(x, _) y -> (x, y)) :: Lens (c, a) (c, b) a b

-- | Get the value out of structure.
--
-- >>> view sndLens (1, 2)
-- 2
view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- | Set the value inside a structure.
--
-- >>> set sndLens 3 (1, 2)
-- (1,3)
set :: Lens s t a b -> b -> s -> t
set l a = runIdentity . l (const (Identity a))

-- | Apply a function to the value inside a structure.
--
-- >>> over sndLens (*3) (1, 2)
-- (1,6)
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
