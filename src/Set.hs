-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.Set".
module Set
  ( -- * IntMap
    Set,
    null,
    size,

    -- * Construction
    empty,
    singleton,
    powerSet,

    -- ** From Unordered Lists
    fromList,

    -- ** From Ascending Lists
    fromAscList,
    fromDistinctAscList,

    -- ** From Descending Lists
    fromDescList,
    fromDistinctDescList,

    -- * Query
    member,
    notMember,

    -- ** Lookup Ordered
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    lookupMin,
    lookupMax,

    -- ** Lookup Index
    lookupIndex,

    -- ** Subset
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
    disjointUnion,

    -- ** Difference
    difference,
    (\\),

    -- ** Intersection
    intersection,

    -- ** Cartesian
    cartesianProduct,

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

    -- ** Lists
    toList,
    toAscList,
    toDescList,

    -- * Filter
    filter,

    -- * Partition
    partition,

    -- * Split
    take,
    drop,
    split,
    splitAt,
    splitMember,
    splitRoot,
  )
where

import Data.Set