module GHC.Rename.Splice where

import GHC.Prelude
import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Types.Name.Set


rnSpliceType :: HsUntypedSplice GhcPs -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsUntypedSplice GhcPs -> RnM (Either (HsUntypedSplice GhcRn, HsUntypedSpliceResult (Pat GhcPs))
                                                     (Pat GhcRn)
                                            , FreeVars)
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsUntypedSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
