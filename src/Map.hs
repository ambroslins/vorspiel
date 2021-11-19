-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.Map.Stict".
module Map
  ( module Data.Map.Strict,
  )
where

import Data.Map.Strict hiding
  ( deleteAt,
    deleteFindMax,
    deleteFindMin,
    elemAt,
    findIndex,
    findMax,
    findMin,
    updateAt,
    (!),
  )
