-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.Text".
module Text
  ( -- * Text
    Text,

    -- * Accessors

    -- ** Length information
    length,
    null,

    -- ** Subtext
    take,
    takeEnd,
    drop,
    dropEnd,
    uncons,
    unsnoc,

    -- *** Stripping
    strip,
    stripStart,
    stripEnd,

    -- * Construction
    empty,
    singleton,
    pack,
    replicate,

    -- ** Concatenation
    cons,
    snoc,
    append,
    concat,
    concatMap,

    -- ** Unfolds
    unfoldr,
    unfoldrN,

    -- * Modifying Text

    -- ** Map
    map,

    -- ** Case conversion
    toCaseFold,
    toLower,
    toUpper,
    toTitle,

    -- ** Transformations
    reverse,
    intersperse,
    intercalate,
    inits,
    tails,

    -- ** Justification
    justifyLeft,
    justifyRight,
    center,

    -- * Folds
    foldl,
    foldl',
    foldr,

    -- ** Specialised folds
    any,
    all,

    -- * Scans
    scanl,
    scanr,

    -- * Working with predicates

    -- ** Filter
    filter,

    -- ** Partition
    partition,
    span,
    break,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    dropAround,
    group,
    groupBy,

    -- ** Substrings
    split,
    splitOn,
    chunksOf,

    -- * Lines and Words
    lines,
    words,
    unlines,
    unwords,

    -- * Predicates
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * Searching
    elem,
    find,

    -- ** View Patterns
    stripPrefix,
    stripSuffix,
    commonPrefixes,

    -- * Zipping
    zip,
    zipWith,

    -- * Low level operations
    copy,
  )
where

import Data.Text