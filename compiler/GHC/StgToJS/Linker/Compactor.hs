{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- The compactor does link-time optimization. It is much simpler than the
-- Optimizer, no fancy dataflow analysis here.
--
-- Optimizations:
-- - rewrite all variables starting with h$$ to shorter names, these are internal names
-- - write all function metadata compactly
--
-- TODO: Jeff (2022,03): adapt to GHC Head. We skip this for now and live with
-- an unoptimized linker. Once the Linker and RTS are up and running return to
-- this and optimize
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Compactor where

