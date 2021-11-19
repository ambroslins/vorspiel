-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.IntMap.Strict".
module IntMap
  ( module Data.IntMap.Strict,
  )
where

import Data.IntMap.Strict hiding
  ( deleteFindMax,
    deleteFindMin,
    findMax,
    findMin,
    (!),
  )
