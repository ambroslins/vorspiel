-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.HashMap.Strict".
module HashMap
  ( -- * HashMap
    HashMap,
    null,
    size,

    -- * Construction
    empty,
    singleton,
    fromSet,

    -- ** From Lists
    fromList,
    fromListWith,
    fromListWithKey,

    -- * Query
    lookup,
    findWithDefault,
    (!?),
    member,
    notMember,
    isSubmapOf,
    isSubmapOfBy,

    -- * Insert
    insert,
    insertWith,

    -- * Update
    delete,
    adjust,
    update,
    alter,
    alterF,

    -- * Combine

    -- ** Union
    union,
    unionWith,
    unionWithKey,
    unions,

    -- ** Difference
    difference,
    differenceWith,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,

    -- ** Compose
    compose,

    -- * Map
    map,
    mapWithKey,
    traverseWithKey,
    mapKeys,

    -- * Fold

    -- ** Lazy
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,

    -- ** Strict
    foldr',
    foldl',
    foldrWithKey',
    foldlWithKey',

    -- * Conversions
    elems,
    keys,
    toList,
    keysSet,

    -- * Filter
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,
  )
where

import Data.HashMap.Strict
import Data.HashSet (HashSet, toMap)
import Data.Hashable (Hashable)
import Vorspiel.Prelude (Bool (..), Eq, const, not, (.))

-- | /O(n)/ Build a 'HashMap' from a 'HashSet' by appyling a function to each key.
fromSet :: (k -> v) -> HashSet k -> HashMap k v
fromSet f = mapWithKey (const . f) . toMap

-- | /O(log n)/ Return 'False' if the specified key is present in the map, 'True' otherwise.
-- See also 'member'.
notMember :: (Eq k, Hashable k) => k -> HashMap k v -> Bool
notMember k m = not (member k m)
