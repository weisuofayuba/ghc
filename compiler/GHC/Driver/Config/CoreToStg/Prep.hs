module GHC.Driver.Config.CoreToStg.Prep
  ( initCorePrepConfig
  , initCorePrepPgmConfig
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Tc.Utils.Env

import GHC.CoreToStg.Prep

initCorePrepConfig :: HscEnv -> IO CorePrepConfig
initCorePrepConfig hsc_env = do
   convertNumLit <- do
     let platform = targetPlatform $ hsc_dflags hsc_env
         home_unit = hsc_home_unit hsc_env
         lookup_global = lookupGlobal hsc_env
     mkConvertNumLiteral platform home_unit lookup_global
   return $ CorePrepConfig
      { cp_catchNonexhaustiveCases = gopt Opt_CatchNonexhaustiveCases $ hsc_dflags hsc_env
      , cp_convertNumLit = convertNumLit
      }

initCorePrepPgmConfig :: DynFlags -> CorePrepPgmConfig
initCorePrepPgmConfig dflags = CorePrepPgmConfig
  { cpPgm_dumpCoreSizes     = debugLevel dflags == 0
  , cpPgm_generateDebugInfo = not $ gopt Opt_SuppressCoreSizes dflags
  }
