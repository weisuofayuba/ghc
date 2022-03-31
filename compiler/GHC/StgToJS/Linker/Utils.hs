{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Utils
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Various utilies used in the JS Linker
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Utils where

import           System.FilePath
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           System.IO (withBinaryFile, IOMode(WriteMode))

import          GHC.Data.ShortText
import          GHC.Unit.State
import          GHC.Unit.Types

import           Prelude

addExeExtension :: FilePath -> FilePath
addExeExtension = id
{- FIXME: Jeff (2022,03): with FIXME: after Windows FIXME in Linker, fix this too
  | Platform.isWindows = (<.> "exe")
  | otherwise          = id
-}

{-
      macOS has trouble writing more than 2GiB at once to a file
  (tested with 10.14.6), and the base library doesn't work around this
  problem yet (tested with GHC 8.6), so we work around it here.

  in this workaround we write a binary file in chunks of 1 GiB
  FIXME: Jeff (2022,03): Is this still true?
 -}
writeBinaryFile :: FilePath -> ByteString -> IO ()
writeBinaryFile file bs =
  withBinaryFile file WriteMode $ \h -> mapM_ (B.hPut h) (chunks bs)
  where
    -- split the ByteString into a nonempty list of chunks of at most 1GiB
    chunks :: ByteString -> [ByteString]
    chunks b =
      let (b1, b2) = B.splitAt 1073741824 b
      in  b1 : if B.null b1 then [] else chunks b2

getInstalledPackageLibDirs :: UnitState -> UnitId -> [FilePath]
getInstalledPackageLibDirs us = fmap unpack . maybe mempty unitLibraryDirs . lookupUnitId us

getInstalledPackageHsLibs :: UnitState -> UnitId -> [String]
getInstalledPackageHsLibs us = fmap unpack . maybe mempty unitLibraries . lookupUnitId us
