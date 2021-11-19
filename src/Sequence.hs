-- |
-- Copyright   : (c) 2021, Ambros Lins
-- License     : BSD-3-Clause
-- Maintainer  : ambros.lins@gmail.com
--
-- This module exports safe functions from "Data.Sequence".
module Sequence
  ( module Data.Sequence,
  )
where

import Data.Sequence hiding
  ( cycleTaking,
    index,
  )
