-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.IntMap.Strict".
module IntMap
  ( -- * IntMap
    IntMap,
    Key,
    null,
    size,

    -- * Construction
    empty,
    singleton,
    fromSet,

    -- ** From Unordered Lists
    fromList,
    fromListWith,
    fromListWithKey,

    -- ** From Ascending Lists
    fromAscList,
    fromAscListWith,
    fromAscListWithKey,
    fromDistinctAscList,

    -- * Query
    lookup,
    (!?),
    findWithDefault,
    member,
    notMember,

    -- ** Lookup Ordered
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    lookupMin,
    lookupMax,

    -- ** Submap
    isSubmapOf,
    isSubmapOfBy,
    isProperSubmapOf,
    isProperSubmapOfBy,

    -- ** Disjoint
    disjoint,

    -- * Insert
    insert,
    insertWith,
    insertWithKey,
    insertLookupWithKey,

    -- * Delete
    delete,
    deleteMin,
    deleteMax,

    -- * Update
    adjust,
    adjustWithKey,
    update,
    updateWithKey,
    updateMin,
    updateMax,
    updateMinWithKey,
    updateMaxWithKey,
    alter,
    alterF,

    -- * View
    minView,
    maxView,
    minViewWithKey,
    maxViewWithKey,

    -- * Combine

    -- ** Union
    union,
    unionWith,
    unionWithKey,
    unions,
    unionsWith,

    -- ** Difference
    difference,
    (\\),
    differenceWith,
    differenceWithKey,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,

    -- ** Compose
    compose,

    -- ** Merge
    mergeWithKey,

    -- * Map
    map,
    mapWithKey,

    -- ** Traverse
    traverseWithKey,
    traverseMaybeWithKey,

    -- ** Accumulating
    mapAccum,
    mapAccumWithKey,
    mapAccumRWithKey,

    -- ** Map Keys
    mapKeys,
    mapKeysWith,
    mapKeysMonotonic,

    -- ** Filter Map
    mapMaybe,
    mapMaybeWithKey,
    mapEither,
    mapEitherWithKey,

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
    keysSet,

    -- ** Lists
    toList,
    assocs,
    toAscList,
    toDescList,

    -- * Filter
    filter,
    filterWithKey,
    restrictKeys,
    withoutKeys,

    -- * Partition
    partition,
    partitionWithKey,

    -- * Split
    split,
    splitLookup,
    splitRoot,
  )
where

import Data.IntMap.Strict
