-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.HashSet".
module HashSet
  ( -- * HashSet
    HashSet,
    null,
    size,

    -- * Construction
    empty,
    singleton,
    fromList,

    -- * Query
    member,
    isSubsetOf,

    -- * Insert
    insert,

    -- * Update
    delete,

    -- * Combine
    union,
    unions,
    difference,
    intersection,

    -- * Map
    map,

    -- * Fold
    foldl',
    foldr,

    -- * Conversions
    toMap,
    fromMap,

    -- * Filter
    filter,
  )
where

import Data.HashSet
