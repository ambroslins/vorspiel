-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.Sequence".
module Sequence
  ( -- * Sequence
    Seq (..),

    -- * Accessors

    -- ** Length information
    length,
    null,

    -- ** Indexing
    lookup,
    (!?),

    -- ** Views
    viewl,
    ViewL (..),
    viewr,
    ViewR (..),

    -- ** Subsequences
    tails,
    inits,
    chunksOf,
    take,
    drop,
    splitAt,

    -- * Construction
    empty,
    singleton,
    (<|),
    (|>),
    (><),
    fromList,
    fromFunction,
    fromArray,
    replicate,
    replicateA,

    -- ** Iterative Construction
    iterateN,
    unfoldr,
    unfoldl,

    -- * Modifying
    adjust,
    adjust',
    update,
    insertAt,
    deleteAt,

    -- * Transformations
    mapWithIndex,
    traverseWithIndex,
    reverse,
    intersperse,

    -- * Folds
    foldMapWithIndex,
    foldlWithIndex,
    foldrWithIndex,

    -- * Scans
    scanl,
    scanr,

    -- * Working with predicates

    -- ** Filter
    filter,

    -- ** Partition
    partition,
    takeWhileL,
    takeWhileR,
    dropWhileL,
    dropWhileR,
    spanl,
    spanr,
    breakl,
    breakr,

    -- ** Searching
    elemIndexL,
    elemIndicesL,
    elemIndexR,
    elemIndicesR,
    findIndexL,
    findIndicesL,
    findIndexR,
    findIndicesR,

    -- * Sorting
    sort,
    sortBy,
    sortOn,
    unstableSort,
    unstableSortBy,
    unstableSortOn,

    -- * Zipping
    zip,
    zipWith,
    zip3,
    zipWith3,
    zip4,
    zipWith4,
    unzip,
    unzipWith,
  )
where

import Data.Sequence
