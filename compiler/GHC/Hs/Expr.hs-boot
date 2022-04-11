{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Expr where

import GHC.Utils.Outputable ( SDoc, Outputable )
import Language.Haskell.Syntax.Pat ( LPat )
import {-# SOURCE #-} GHC.Hs.Pat () -- for Outputable
import Language.Haskell.Syntax.Extension ( IdP )
import Language.Haskell.Syntax.Expr
  ( HsExpr, LHsExpr
  , HsCmd
  , MatchGroup
  , GRHSs
  , HsUntypedSplice
  )
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )
import Data.Kind  ( Type )
import Data.Bool  ( Bool )

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p))
instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p))

pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprTypedSplice   :: (OutputableBndrId p) => IdP (GhcPass p) -> LHsExpr (GhcPass p) -> SDoc
pprUntypedSplice :: (OutputableBndrId p) => Bool -> HsUntypedSplice (GhcPass p) -> SDoc

pprPatBind :: forall bndr p . (OutputableBndrId bndr,
                               OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc

pprFunBind :: (OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc

type role HsUntypedSpliceResult representational
data HsUntypedSpliceResult (cts :: Type)
