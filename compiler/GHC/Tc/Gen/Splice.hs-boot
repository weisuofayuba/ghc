{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Gen.Splice where

import GHC.Prelude
import GHC.Types.Name
import GHC.Hs.Expr ( PendingRnSplice, DelayedSplice )
import GHC.Tc.Types( TcM , SpliceType )
import GHC.Tc.Utils.TcType   ( ExpRhoType )
import GHC.Types.Annotations ( Annotation, CoreAnnTarget )
import GHC.Hs.Extension ( GhcRn, GhcPs, GhcTc )

import GHC.Hs     ( HsQuote, HsExpr, LHsExpr, LHsType, LPat, LHsDecl,
                    ThModFinalizers, TypedSpliceRn )
import qualified Language.Haskell.TH as TH

tcTypedSplice :: TypedSpliceRn
              -> LHsExpr GhcRn
              -> ExpRhoType
              -> TcM (HsExpr GhcTc)

tcTypedBracket :: HsExpr GhcRn
               -> LHsExpr GhcRn
               -> ExpRhoType
               -> TcM (HsExpr GhcTc)
tcUntypedBracket :: HsExpr GhcRn
                 -> HsQuote GhcRn
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr GhcTc)

runTopSplice :: DelayedSplice -> TcM (HsExpr GhcTc)

runAnnotation     :: CoreAnnTarget -> LHsExpr GhcRn -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GhcTc) -> TcM (LHsExpr GhcTc)

runMetaE :: LHsExpr GhcTc -> TcM (LHsExpr GhcPs)
runMetaP :: LHsExpr GhcTc -> TcM (LPat GhcPs)
runMetaT :: LHsExpr GhcTc -> TcM (LHsType GhcPs)
runMetaD :: LHsExpr GhcTc -> TcM [LHsDecl GhcPs]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
