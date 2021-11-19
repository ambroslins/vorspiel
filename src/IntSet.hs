-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.IntSet".
module IntSet
  ( module Data.IntSet,
  )
where

import Data.IntSet hiding
  ( deleteFindMax,
    deleteFindMin,
    findMax,
    findMin,
  )
