module GHC.Driver.Config.HsToCore.Coverage
  ( initCoverageConfig
  )
where

import GHC.Prelude

import Data.Maybe (catMaybes)

import GHC.Driver.Backend
import GHC.Driver.Session
import GHC.Runtime.Interpreter.Types
import GHC.HsToCore.Coverage

initCoverageConfig :: Maybe Interp -> DynFlags -> CoverageConfig
initCoverageConfig m_interp dflags = CoverageConfig
  { coverage_passes       = coveragePasses m_interp dflags
  , coverage_profAuto     = profAuto dflags
  , coverage_countEntries = gopt Opt_ProfCountEntries dflags
  , coverage_hpc          = if gopt Opt_Hpc dflags then Just $ hpcDir dflags else Nothing
  }

sourceNotesEnabled :: DynFlags -> Bool
sourceNotesEnabled dflags =
  (debugLevel dflags > 0) || (gopt Opt_InfoTableMap dflags)

coveragePasses :: Maybe Interp -> DynFlags -> [TickishType]
coveragePasses m_interp dflags = catMaybes
  [ case (m_interp, backend dflags) of
      (Just interp, Interpreter) -> Just $ Breakpoints interp
      _                          -> Nothing
  , ifA HpcTicks $ gopt Opt_Hpc dflags
  , ifA ProfNotes $ sccProfilingEnabled dflags && profAuto dflags /= NoProfAuto
  , ifA SourceNotes $ sourceNotesEnabled dflags
  ]
  where ifA x cond = if cond then Just x else Nothing
