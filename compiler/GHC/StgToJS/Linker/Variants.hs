{-# OPTIONS_GHC -Wno-unused-imports #-}
-- |

{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module GHC.StgToJS.Linker.Variants where

import           Data.ByteString       (ByteString)
import           Data.Set              (Set)

import qualified GHC.StgToJS.CodeGen       as Gen2
import qualified GHC.StgToJS.Linker.Linker as Gen2
import qualified GHC.StgToJS.Object        as Gen2
import Prelude

import GHC.Types.CostCentre
import GHC.Unit.Types ( Module, UnitId )
import GHC.Stg.Syntax
import GHC.Linker.Types (SptEntry)
import GHC.Types.ForeignStubs (ForeignStubs (..), getCHeader, getCStub)
import GHC.Driver.Session
import GHC.StgToJS.Linker.Types
import GHC.Utils.Logger
import GHC.StgToJS.Types
import GHC.Driver.Env
import GHC.StgToJS.Object

data Variant = Variant
    { variantRender :: Logger
                    -> StgToJSConfig
                    -> [CgStgTopBinding]
                    -> Module
                    -> [SptEntry]
                    -> ForeignStubs
                    -> CollectedCCs
                    -> FilePath               -- ^ Output file name -> IO ()
                    -> IO ()
    , variantLink   :: HscEnv
                    -> GhcjsEnv
                    -> JSLinkConfig
                    -> StgToJSConfig
                    -> FilePath               -- ^ output file or directory
                    -> [FilePath]             -- ^ include path for home package
                    -> [UnitId]               -- ^ packages to link
                    -> [LinkedObj]            -- ^ the object files we're linking
                    -> [FilePath]             -- ^ extra js files to include
                    -> (ExportedFun -> Bool)  -- ^ functions from the objects to use as roots (include all their deps)
                    -> Set ExportedFun        -- ^ extra symbols to link in
                    -> IO ()
    }

gen2Variant :: Variant
gen2Variant = Variant Gen2.stgToJS Gen2.link

type StgPgm = [StgBinding]
