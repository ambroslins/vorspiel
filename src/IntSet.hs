-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.IntSet".
module IntSet
  ( -- * IntSet
    IntSet,
    Key,

    -- * Construction
    empty,
    singleton,

    -- ** From Lists
    fromList,
    fromAscList,
    fromDistinctAscList,

    -- * Query
    member,
    notMember,

    -- ** Lookup Ordered
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,

    -- ** Submap
    isSubsetOf,
    isProperSubsetOf,

    -- ** Disjoint
    disjoint,

    -- * Insert
    insert,

    -- * Delete
    delete,
    deleteMin,
    deleteMax,

    -- * Update
    alterF,

    -- * View
    minView,
    maxView,

    -- * Combine

    -- ** Union
    union,
    unions,

    -- ** Difference
    difference,
    (\\),

    -- ** Intersection
    intersection,

    -- * Map
    map,
    mapMonotonic,

    -- * Fold

    -- ** Lazy
    foldr,
    foldl,

    -- ** Strict
    foldr',
    foldl',

    -- * Conversions
    elems,
    toList,
    toAscList,
    toDescList,

    -- * Filter
    filter,

    -- * Partition
    partition,

    -- * Split
    split,
    splitRoot,
  )
where

import Data.IntSet