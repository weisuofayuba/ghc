{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains exclusively Data instances, which are going to be slow
-- no matter what we do. Furthermore, they are incredibly slow to compile with
-- optimisation (see #9557). Consequently we compile this with -O0.
-- See #18254.
{-# OPTIONS_GHC -O0 #-}

module GHC.Hs.Instances where

-- This module defines the Data instances for the hsSyn AST.

-- It happens here to avoid massive constraint types on the AST with concomitant
-- slow GHC bootstrap times.

-- UndecidableInstances ?

import Data.Data hiding ( Fixity )

import GHC.Prelude
import GHC.Hs.Extension
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Lit
import GHC.Hs.Type
import GHC.Hs.Pat
import GHC.Hs.ImpExp
import GHC.Parser.Annotation

-- ---------------------------------------------------------------------
-- Data instance CPP macros    -----------------------------------------

#define DATA_IMPL_GHC_PASS \
  { gfoldl     = case ghcPass @p of { GhcPs -> gfoldl;     GhcRn -> gfoldl;     GhcTc -> gfoldl     } \
  ; gunfold    = case ghcPass @p of { GhcPs -> gunfold;    GhcRn -> gunfold;    GhcTc -> gunfold    } \
  ; toConstr   = case ghcPass @p of { GhcPs -> toConstr;   GhcRn -> toConstr;   GhcTc -> toConstr   } \
  ; dataTypeOf = case ghcPass @p of { GhcPs -> dataTypeOf; GhcRn -> dataTypeOf; GhcTc -> dataTypeOf } \
  ; dataCast1  = case ghcPass @p of { GhcPs -> dataCast1;  GhcRn -> dataCast1;  GhcTc -> dataCast1  } \
  ; dataCast2  = case ghcPass @p of { GhcPs -> dataCast2;  GhcRn -> dataCast2;  GhcTc -> dataCast2  } \
  ; gmapT      = case ghcPass @p of { GhcPs -> gmapT;      GhcRn -> gmapT;      GhcTc -> gmapT      } \
  ; gmapQl     = case ghcPass @p of { GhcPs -> gmapQl;     GhcRn -> gmapQl;     GhcTc -> gmapQl     } \
  ; gmapQr     = case ghcPass @p of { GhcPs -> gmapQr;     GhcRn -> gmapQr;     GhcTc -> gmapQr     } \
  ; gmapQ      = case ghcPass @p of { GhcPs -> gmapQ;      GhcRn -> gmapQ;      GhcTc -> gmapQ      } \
  ; gmapQi     = case ghcPass @p of { GhcPs -> gmapQi;     GhcRn -> gmapQi;     GhcTc -> gmapQi     } \
  ; gmapM      = case ghcPass @p of { GhcPs -> gmapM;      GhcRn -> gmapM;      GhcTc -> gmapM      } \
  ; gmapMp     = case ghcPass @p of { GhcPs -> gmapMp;     GhcRn -> gmapMp;     GhcTc -> gmapMp     } \
  ; gmapMo     = case ghcPass @p of { GhcPs -> gmapMo;     GhcRn -> gmapMo;     GhcTc -> gmapMo     } }

#define DATA_INST(T) \
deriving instance Data (T GhcPs); \
deriving instance Data (T GhcRn); \
deriving instance Data (T GhcTc); \
instance {-# OVERLAPPABLE #-} (Typeable p, IsPass p) => Data (T (GhcPass p)) where DATA_IMPL_GHC_PASS

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs-----------------------------------------

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Binds ----------------------------------

-- deriving instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
deriving instance Data (HsLocalBindsLR GhcPs GhcPs)
deriving instance Data (HsLocalBindsLR GhcPs GhcRn)
deriving instance Data (HsLocalBindsLR GhcRn GhcRn)
deriving instance Data (HsLocalBindsLR GhcTc GhcTc)
instance {-# OVERLAPPABLE #-} (Typeable p, IsPass p) => Data (HsLocalBindsLR (GhcPass p) (GhcPass p)) where DATA_IMPL_GHC_PASS

-- deriving instance (DataIdLR pL pR) => Data (HsValBindsLR pL pR)
deriving instance Data (HsValBindsLR GhcPs GhcPs)
deriving instance Data (HsValBindsLR GhcPs GhcRn)
deriving instance Data (HsValBindsLR GhcRn GhcRn)
deriving instance Data (HsValBindsLR GhcTc GhcTc)

-- deriving instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
deriving instance Data (NHsValBindsLR GhcPs)
deriving instance Data (NHsValBindsLR GhcRn)
deriving instance Data (NHsValBindsLR GhcTc)

-- deriving instance (DataIdLR pL pR) => Data (HsBindLR pL pR)
deriving instance Data (HsBindLR GhcPs GhcPs)
deriving instance Data (HsBindLR GhcPs GhcRn)
deriving instance Data (HsBindLR GhcRn GhcRn)
deriving instance Data (HsBindLR GhcTc GhcTc)

DATA_INST(ABExport)
DATA_INST(RecordPatSynField)

-- deriving instance (DataIdLR pL pR) => Data (PatSynBind pL pR)
deriving instance Data (PatSynBind GhcPs GhcPs)
deriving instance Data (PatSynBind GhcPs GhcRn)
deriving instance Data (PatSynBind GhcRn GhcRn)
deriving instance Data (PatSynBind GhcTc GhcTc)

DATA_INST(HsIPBinds)
DATA_INST(IPBind)
DATA_INST(Sig)
DATA_INST(FixitySig)
DATA_INST(StandaloneKindSig)
DATA_INST(HsPatSynDir)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Decls ----------------------------------

DATA_INST(HsDecl)
DATA_INST(HsGroup)
DATA_INST(SpliceDecl)
DATA_INST(TyClDecl)
DATA_INST(FunDep)
DATA_INST(TyClGroup)
DATA_INST(FamilyResultSig)
DATA_INST(FamilyDecl)
DATA_INST(InjectivityAnn)
DATA_INST(FamilyInfo)
DATA_INST(HsDataDefn)
DATA_INST(HsDerivingClause)
DATA_INST(DerivClauseTys)
DATA_INST(ConDecl)
DATA_INST(HsConDeclGADTDetails)
DATA_INST(TyFamInstDecl)
DATA_INST(DataFamInstDecl)

-- deriving instance (DataIdLR p p,Data rhs)=>Data (FamEqn p rhs)
deriving instance Data rhs => Data (FamEqn GhcPs rhs)
deriving instance Data rhs => Data (FamEqn GhcRn rhs)
deriving instance Data rhs => Data (FamEqn GhcTc rhs)

DATA_INST(ClsInstDecl)
DATA_INST(InstDecl)
DATA_INST(DerivDecl)
DATA_INST(DerivStrategy)
DATA_INST(DefaultDecl)
DATA_INST(ForeignDecl)
DATA_INST(RuleDecls)
DATA_INST(RuleDecl)
DATA_INST(RuleBndr)
DATA_INST(WarnDecls)
DATA_INST(WarnDecl)
DATA_INST(AnnProvenance)
DATA_INST(AnnDecl)
DATA_INST(RoleAnnotDecl)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Expr -----------------------------------

DATA_INST(FieldLabelStrings)
DATA_INST(DotFieldOcc)
DATA_INST(HsPragE)
DATA_INST(HsExpr)
DATA_INST(HsTupArg)
DATA_INST(HsCmd)
DATA_INST(HsCmdTop)

-- deriving instance (DataIdLR p p,Data body) => Data (MatchGroup p body)
deriving instance Data (MatchGroup GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (MatchGroup GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (MatchGroup GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (MatchGroup GhcPs (LocatedA (HsCmd GhcPs)))
deriving instance Data (MatchGroup GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (MatchGroup GhcTc (LocatedA (HsCmd GhcTc)))

-- deriving instance (DataIdLR p p,Data body) => Data (Match      p body)
deriving instance Data (Match      GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (Match      GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (Match      GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (Match      GhcPs (LocatedA (HsCmd GhcPs)))
deriving instance Data (Match      GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (Match      GhcTc (LocatedA (HsCmd GhcTc)))
instance {-# OVERLAPPABLE #-} (Typeable p, IsPass p, Data body) => Data (Match (GhcPass p) body) where DATA_IMPL_GHC_PASS

-- deriving instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
deriving instance Data (GRHSs     GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (GRHSs     GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (GRHSs     GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (GRHSs     GhcPs (LocatedA (HsCmd GhcPs)))
deriving instance Data (GRHSs     GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (GRHSs     GhcTc (LocatedA (HsCmd GhcTc)))

-- deriving instance (DataIdLR p p,Data body) => Data (GRHS       p body)
deriving instance Data (GRHS     GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (GRHS     GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (GRHS     GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (GRHS     GhcPs (LocatedA (HsCmd GhcPs)))
deriving instance Data (GRHS     GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (GRHS     GhcTc (LocatedA (HsCmd GhcTc)))
instance {-# OVERLAPPABLE #-} (Typeable p, IsPass p, Data body) => Data (GRHS (GhcPass p) body) where DATA_IMPL_GHC_PASS

-- deriving instance (DataIdLR p p,Data body) => Data (StmtLR   p p body)
deriving instance Data (StmtLR   GhcPs GhcPs (LocatedA (HsExpr GhcPs)))
deriving instance Data (StmtLR   GhcPs GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (StmtLR   GhcRn GhcRn (LocatedA (HsExpr GhcRn)))
deriving instance Data (StmtLR   GhcTc GhcTc (LocatedA (HsExpr GhcTc)))
deriving instance Data (StmtLR   GhcPs GhcPs (LocatedA (HsCmd GhcPs)))
deriving instance Data (StmtLR   GhcPs GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (StmtLR   GhcRn GhcRn (LocatedA (HsCmd GhcRn)))
deriving instance Data (StmtLR   GhcTc GhcTc (LocatedA (HsCmd GhcTc)))
instance {-# OVERLAPPABLE #-} (Typeable p, IsPass p, Data body) => Data (StmtLR (GhcPass p) (GhcPass p) body) where DATA_IMPL_GHC_PASS

deriving instance Data RecStmtTc

-- deriving instance (DataIdLR p p) => Data (ParStmtBlock p p)
deriving instance Data (ParStmtBlock GhcPs GhcPs)
deriving instance Data (ParStmtBlock GhcPs GhcRn)
deriving instance Data (ParStmtBlock GhcRn GhcRn)
deriving instance Data (ParStmtBlock GhcTc GhcTc)

DATA_INST(ApplicativeArg)
DATA_INST(HsStmtContext)

deriving instance Data HsDoFlavour

DATA_INST(HsMatchContext)
DATA_INST(HsSplice)
DATA_INST(HsSplicedThing)
DATA_INST(HsBracket)
DATA_INST(ArithSeqInfo)

deriving instance Data RecordUpdTc
deriving instance Data CmdTopTc
deriving instance Data PendingRnSplice
deriving instance Data PendingTcSplice
deriving instance Data SyntaxExprRn
deriving instance Data SyntaxExprTc

deriving instance Data XBindStmtRn
deriving instance Data XBindStmtTc

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Lit ------------------------------------

DATA_INST(HsLit)
DATA_INST(HsOverLit)

deriving instance Data OverLitRn
deriving instance Data OverLitTc

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Pat ------------------------------------

DATA_INST(Pat)

deriving instance Data ConPatTc

deriving instance (Data a, Data b) => Data (HsFieldBind a b)

deriving instance (Data body) => Data (HsRecFields GhcPs body)
deriving instance (Data body) => Data (HsRecFields GhcRn body)
deriving instance (Data body) => Data (HsRecFields GhcTc body)

-- ---------------------------------------------------------------------
-- Data derivations from GHC.Hs.Type ----------------------------------

DATA_INST(LHsQTyVars)

-- deriving instance (Data flag, DataIdLR p p) => Data (HsOuterTyVarBndrs p)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcPs)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcRn)
deriving instance Data flag => Data (HsOuterTyVarBndrs flag GhcTc)

DATA_INST(HsSigType)

-- deriving instance (DataIdLR p p, Data thing) =>Data (HsWildCardBndrs p thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcPs thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcRn thing)
deriving instance (Data thing) => Data (HsWildCardBndrs GhcTc thing)

DATA_INST(HsPatSigType)
DATA_INST(HsForAllTelescope)

-- deriving instance (DataIdLR p p) => Data (HsTyVarBndr p)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcPs)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcRn)
deriving instance (Data flag) => Data (HsTyVarBndr flag GhcTc)

DATA_INST(HsType)
DATA_INST(HsLinearArrowTokens)
DATA_INST(HsArrow)

-- deriving instance (DataIdLR p p) => Data (HsScaled p a)
deriving instance Data thing => Data (HsScaled GhcPs thing)
deriving instance Data thing => Data (HsScaled GhcRn thing)
deriving instance Data thing => Data (HsScaled GhcTc thing)

deriving instance (Data a, Data b) => Data (HsArg a b)

DATA_INST(ConDeclField)
DATA_INST(FieldOcc)
DATA_INST(AmbiguousFieldOcc)
DATA_INST(ImportDecl)
DATA_INST(IE)

-- deriving instance (Eq name, Eq (IdP name)) => Eq (IE name)
deriving instance Eq (IE GhcPs)
deriving instance Eq (IE GhcRn)
deriving instance Eq (IE GhcTc)

-- ---------------------------------------------------------------------

deriving instance Data XXExprGhcTc
deriving instance Data XXPatGhcTc

-- ---------------------------------------------------------------------

deriving instance Data XViaStrategyPs

-- ---------------------------------------------------------------------
