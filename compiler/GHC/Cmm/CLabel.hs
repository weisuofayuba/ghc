-----------------------------------------------------------------------------
--
-- Object-file symbols (called CLabel for histerical raisins).
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Cmm.CLabel (
        CLabel, -- abstract type
        NeedExternDecl (..),
        ForeignLabelSource(..),
        pprDebugCLabel,

        mkClosureLabel,
        mkSRTLabel,
        mkInfoTableLabel,
        mkEntryLabel,
        mkRednCountsLabel,
        mkConInfoTableLabel,
        mkApEntryLabel,
        mkApInfoTableLabel,
        mkClosureTableLabel,
        mkBytesLabel,

        mkLocalBlockLabel,
        mkLocalClosureLabel,
        mkLocalInfoTableLabel,
        mkLocalClosureTableLabel,

        mkBlockInfoTableLabel,

        mkBitmapLabel,
        mkStringLitLabel,

        mkAsmTempLabel,
        mkAsmTempDerivedLabel,
        mkAsmTempEndLabel,
        mkAsmTempDieLabel,

        mkDirty_MUT_VAR_Label,
        mkNonmovingWriteBarrierEnabledLabel,
        mkUpdInfoLabel,
        mkBHUpdInfoLabel,
        mkIndStaticInfoLabel,
        mkMainCapabilityLabel,
        mkMAP_FROZEN_CLEAN_infoLabel,
        mkMAP_FROZEN_DIRTY_infoLabel,
        mkMAP_DIRTY_infoLabel,
        mkSMAP_FROZEN_CLEAN_infoLabel,
        mkSMAP_FROZEN_DIRTY_infoLabel,
        mkSMAP_DIRTY_infoLabel,
        mkBadAlignmentLabel,
        mkArrWords_infoLabel,
        mkSRTInfoLabel,

        mkTopTickyCtrLabel,
        mkCAFBlackHoleInfoTableLabel,
        mkRtsPrimOpLabel,
        mkRtsSlowFastTickyCtrLabel,

        mkSelectorInfoLabel,
        mkSelectorEntryLabel,

        mkCmmInfoLabel,
        mkCmmEntryLabel,
        mkCmmRetInfoLabel,
        mkCmmRetLabel,
        mkCmmCodeLabel,
        mkCmmDataLabel,
        mkRtsCmmDataLabel,
        mkCmmClosureLabel,

        mkRtsApFastLabel,

        mkPrimCallLabel,

        mkForeignLabel,
        addLabelSize,

        foreignLabelStdcallInfo,
        isBytesLabel,
        isForeignLabel,
        isSomeRODataLabel,
        isStaticClosureLabel,
        mkCCLabel, mkCCSLabel,

        DynamicLinkerLabelInfo(..),
        mkDynamicLinkerLabel,
        dynamicLinkerLabelInfo,

        mkPicBaseLabel,
        mkDeadStripPreventer,

        mkHpcTicksLabel,

        -- * Predicates
        hasCAF,
        needsCDecl, maybeLocalBlockLabel, externallyVisibleCLabel,
        isMathFun,
        isCFunctionLabel, isGcPtrLabel, labelDynamic,
        isLocalCLabel, mayRedirectTo,

        -- * Conversions
        toClosureLbl, toSlowEntryLbl, toEntryLbl, toInfoLbl, hasHaskellName,

        pprCLabel,
        isInfoTableLabel,
        isConInfoTableLabel,
        isIdLabel, isTickyLabel
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Id.Info
import GHC.Types.Basic
import {-# SOURCE #-} GHC.Cmm.BlockId (BlockId, mkBlockId)
import GHC.Unit
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Builtin.PrimOps
import GHC.Types.CostCentre
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Driver.Session
import GHC.Platform
import GHC.Types.Unique.Set
import GHC.Utils.Misc
import GHC.Core.Ppr ( {- instances -} )
import GHC.CmmToAsm.Config

-- -----------------------------------------------------------------------------
-- The CLabel type

{- |
  'CLabel' is an abstract type that supports the following operations:

  - Pretty printing

  - In a C file, does it need to be declared before use?  (i.e. is it
    guaranteed to be already in scope in the places we need to refer to it?)

  - If it needs to be declared, what type (code or data) should it be
    declared to have?

  - Is it visible outside this object file or not?

  - Is it "dynamic" (see details below)

  - Eq and Ord, so that we can make sets of CLabels (currently only
    used in outputting C as far as I can tell, to avoid generating
    more than one declaration for any given label).

  - Converting an info table label into an entry label.

  CLabel usage is a bit messy in GHC as they are used in a number of different
  contexts:

  - By the C-- AST to identify labels

  - By the unregisterised C code generator (\"PprC\") for naming functions (hence
    the name 'CLabel')

  - By the native and LLVM code generators to identify labels

  For extra fun, each of these uses a slightly different subset of constructors
  (e.g. 'AsmTempLabel' and 'AsmTempDerivedLabel' are used only in the NCG and
  LLVM backends).

  In general, we use 'IdLabel' to represent Haskell things early in the
  pipeline. However, later optimization passes will often represent blocks they
  create with 'LocalBlockLabel' where there is no obvious 'Name' to hang off the
  label.
-}

data CLabel
  = -- | A label related to the definition of a particular Id or Con in a .hs file.
    IdLabel
        Name
        CafInfo
        IdLabelInfo             -- ^ encodes the suffix of the label

  -- | A label from a .cmm file that is not associated with a .hs level Id.
  | CmmLabel
        UnitId                  -- ^ what package the label belongs to.
        NeedExternDecl          -- ^ does the label need an "extern .." declaration
        FastString              -- ^ identifier giving the prefix of the label
        CmmLabelInfo            -- ^ encodes the suffix of the label

  -- | A label with a baked-in \/ algorithmically generated name that definitely
  --    comes from the RTS. The code for it must compile into libHSrts.a \/ libHSrts.so
  --    If it doesn't have an algorithmically generated name then use a CmmLabel
  --    instead and give it an appropriate UnitId argument.
  | RtsLabel
        RtsLabelInfo

  -- | A label associated with a block. These aren't visible outside of the
  -- compilation unit in which they are defined. These are generally used to
  -- name blocks produced by Cmm-to-Cmm passes and the native code generator,
  -- where we don't have a 'Name' to associate the label to and therefore can't
  -- use 'IdLabel'.
  | LocalBlockLabel
        {-# UNPACK #-} !Unique

  -- | A 'C' (or otherwise foreign) label.
  --
  | ForeignLabel
        FastString              -- ^ name of the imported label.

        (Maybe Int)             -- ^ possible '@n' suffix for stdcall functions
                                -- When generating C, the '@n' suffix is omitted, but when
                                -- generating assembler we must add it to the label.

        ForeignLabelSource      -- ^ what package the foreign label is in.

        FunctionOrData

  -- | Local temporary label used for native (or LLVM) code generation; must not
  -- appear outside of these contexts. Use primarily for debug information
  | AsmTempLabel
        {-# UNPACK #-} !Unique

  -- | A label \"derived\" from another 'CLabel' by the addition of a suffix.
  -- Must not occur outside of the NCG or LLVM code generators.
  | AsmTempDerivedLabel
        CLabel
        FastString              -- ^ suffix

  | StringLitLabel
        {-# UNPACK #-} !Unique

  | CC_Label  CostCentre
  | CCS_Label CostCentreStack


  -- | These labels are generated and used inside the NCG only.
  --    They are special variants of a label used for dynamic linking
  --    see module PositionIndependentCode for details.
  | DynamicLinkerLabel DynamicLinkerLabelInfo CLabel

  -- | This label is generated and used inside the NCG only.
  --    It is used as a base for PIC calculations on some platforms.
  --    It takes the form of a local numeric assembler label '1'; and
  --    is pretty-printed as 1b, referring to the previous definition
  --    of 1: in the assembler source file.
  | PicBaseLabel

  -- | A label before an info table to prevent excessive dead-stripping on darwin
  | DeadStripPreventer CLabel


  -- | Per-module table of tick locations
  | HpcTicksLabel Module

  -- | Static reference table
  | SRTLabel
        {-# UNPACK #-} !Unique

  -- | A bitmap (function or case return)
  | LargeBitmapLabel
        {-# UNPACK #-} !Unique

  deriving Eq

isIdLabel :: CLabel -> Bool
isIdLabel IdLabel{} = True
isIdLabel _ = False

-- Used in SRT analysis. See Note [Ticky labels in SRT analysis] in
-- GHC.Cmm.Info.Build.
isTickyLabel :: CLabel -> Bool
isTickyLabel (IdLabel _ _ RednCounts) = True
isTickyLabel _ = False

-- | Indicate if "GHC.CmmToC" has to generate an extern declaration for the
-- label (e.g. "extern StgWordArray(foo)").  The type is fixed to StgWordArray.
--
-- Symbols from the RTS don't need "extern" declarations because they are
-- exposed via "includes/Stg.h" with the appropriate type. See 'needsCDecl'.
--
-- The fixed StgWordArray type led to "conflicting types" issues with user
-- provided Cmm files (not in the RTS) that declare data of another type (#15467
-- and test for #17920).  Hence the Cmm parser considers that labels in data
-- sections don't need the "extern" declaration (just add one explicitly if you
-- need it).
--
-- See https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/backends/ppr-c#prototypes
-- for why extern declaration are needed at all.
newtype NeedExternDecl
   = NeedExternDecl Bool
   deriving (Ord,Eq)

-- This is laborious, but necessary. We can't derive Ord because
-- Unique doesn't have an Ord instance. Note nonDetCmpUnique in the
-- implementation. See Note [No Ord for Unique]
-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
instance Ord CLabel where
  compare (IdLabel a1 b1 c1) (IdLabel a2 b2 c2) =
    compare a1 a2 `thenCmp`
    compare b1 b2 `thenCmp`
    compare c1 c2
  compare (CmmLabel a1 b1 c1 d1) (CmmLabel a2 b2 c2 d2) =
    compare a1 a2 `thenCmp`
    compare b1 b2 `thenCmp`
    compare c1 c2 `thenCmp`
    compare d1 d2
  compare (RtsLabel a1) (RtsLabel a2) = compare a1 a2
  compare (LocalBlockLabel u1) (LocalBlockLabel u2) = nonDetCmpUnique u1 u2
  compare (ForeignLabel a1 b1 c1 d1) (ForeignLabel a2 b2 c2 d2) =
    compare a1 a2 `thenCmp`
    compare b1 b2 `thenCmp`
    compare c1 c2 `thenCmp`
    compare d1 d2
  compare (AsmTempLabel u1) (AsmTempLabel u2) = nonDetCmpUnique u1 u2
  compare (AsmTempDerivedLabel a1 b1) (AsmTempDerivedLabel a2 b2) =
    compare a1 a2 `thenCmp`
    compare b1 b2
  compare (StringLitLabel u1) (StringLitLabel u2) =
    nonDetCmpUnique u1 u2
  compare (CC_Label a1) (CC_Label a2) =
    compare a1 a2
  compare (CCS_Label a1) (CCS_Label a2) =
    compare a1 a2
  compare (DynamicLinkerLabel a1 b1) (DynamicLinkerLabel a2 b2) =
    compare a1 a2 `thenCmp`
    compare b1 b2
  compare PicBaseLabel PicBaseLabel = EQ
  compare (DeadStripPreventer a1) (DeadStripPreventer a2) =
    compare a1 a2
  compare (HpcTicksLabel a1) (HpcTicksLabel a2) =
    compare a1 a2
  compare (SRTLabel u1) (SRTLabel u2) =
    nonDetCmpUnique u1 u2
  compare (LargeBitmapLabel u1) (LargeBitmapLabel u2) =
    nonDetCmpUnique u1 u2
  compare IdLabel{} _ = LT
  compare _ IdLabel{} = GT
  compare CmmLabel{} _ = LT
  compare _ CmmLabel{} = GT
  compare RtsLabel{} _ = LT
  compare _ RtsLabel{} = GT
  compare LocalBlockLabel{} _ = LT
  compare _ LocalBlockLabel{} = GT
  compare ForeignLabel{} _ = LT
  compare _ ForeignLabel{} = GT
  compare AsmTempLabel{} _ = LT
  compare _ AsmTempLabel{} = GT
  compare AsmTempDerivedLabel{} _ = LT
  compare _ AsmTempDerivedLabel{} = GT
  compare StringLitLabel{} _ = LT
  compare _ StringLitLabel{} = GT
  compare CC_Label{} _ = LT
  compare _ CC_Label{} = GT
  compare CCS_Label{} _ = LT
  compare _ CCS_Label{} = GT
  compare DynamicLinkerLabel{} _ = LT
  compare _ DynamicLinkerLabel{} = GT
  compare PicBaseLabel{} _ = LT
  compare _ PicBaseLabel{} = GT
  compare DeadStripPreventer{} _ = LT
  compare _ DeadStripPreventer{} = GT
  compare HpcTicksLabel{} _ = LT
  compare _ HpcTicksLabel{} = GT
  compare SRTLabel{} _ = LT
  compare _ SRTLabel{} = GT

-- | Record where a foreign label is stored.
data ForeignLabelSource

   -- | Label is in a named package
   = ForeignLabelInPackage Unit

   -- | Label is in some external, system package that doesn't also
   --   contain compiled Haskell code, and is not associated with any .hi files.
   --   We don't have to worry about Haskell code being inlined from
   --   external packages. It is safe to treat the RTS package as "external".
   | ForeignLabelInExternalPackage

   -- | Label is in the package currently being compiled.
   --   This is only used for creating hacky tmp labels during code generation.
   --   Don't use it in any code that might be inlined across a package boundary
   --   (ie, core code) else the information will be wrong relative to the
   --   destination module.
   | ForeignLabelInThisPackage

   deriving (Eq, Ord)


-- | For debugging problems with the CLabel representation.
--      We can't make a Show instance for CLabel because lots of its components don't have instances.
--      The regular Outputable instance only shows the label name, and not its other info.
--
pprDebugCLabel :: CLabel -> SDoc
pprDebugCLabel lbl
 = case lbl of
        IdLabel _ _ info-> ppr lbl <> (parens $ text "IdLabel"
                                       <> whenPprDebug (text ":" <> text (show info)))
        CmmLabel pkg _ext _name _info
         -> ppr lbl <> (parens $ text "CmmLabel" <+> ppr pkg)

        RtsLabel{}      -> ppr lbl <> (parens $ text "RtsLabel")

        ForeignLabel _name mSuffix src funOrData
            -> ppr lbl <> (parens $ text "ForeignLabel"
                                <+> ppr mSuffix
                                <+> ppr src
                                <+> ppr funOrData)

        _               -> ppr lbl <> (parens $ text "other CLabel")


data IdLabelInfo
  = Closure             -- ^ Label for closure
  | InfoTable           -- ^ Info tables for closures; always read-only
  | Entry               -- ^ Entry point
  | Slow                -- ^ Slow entry point

  | LocalInfoTable      -- ^ Like InfoTable but not externally visible
  | LocalEntry          -- ^ Like Entry but not externally visible

  | RednCounts          -- ^ Label of place to keep Ticky-ticky  info for this Id

  | ConEntry            -- ^ Constructor entry point
  | ConInfoTable        -- ^ Corresponding info table

  | ClosureTable        -- ^ Table of closures for Enum tycons

  | Bytes               -- ^ Content of a string literal. See
                        -- Note [Bytes label].
  | BlockInfoTable      -- ^ Like LocalInfoTable but for a proc-point block
                        -- instead of a closure entry-point.
                        -- See Note [Proc-point local block entry-point].

  deriving (Eq, Ord, Show)


data RtsLabelInfo
  = RtsSelectorInfoTable Bool{-updatable-} Int{-offset-}  -- ^ Selector thunks
  | RtsSelectorEntry     Bool{-updatable-} Int{-offset-}

  | RtsApInfoTable       Bool{-updatable-} Int{-arity-}    -- ^ AP thunks
  | RtsApEntry           Bool{-updatable-} Int{-arity-}

  | RtsPrimOp PrimOp
  | RtsApFast     FastString    -- ^ _fast versions of generic apply
  | RtsSlowFastTickyCtr String

  deriving (Eq, Ord)
  -- NOTE: Eq on PtrString compares the pointer only, so this isn't
  -- a real equality.


-- | What type of Cmm label we're dealing with.
--      Determines the suffix appended to the name when a CLabel.CmmLabel
--      is pretty printed.
data CmmLabelInfo
  = CmmInfo                     -- ^ misc rts info tables,      suffix _info
  | CmmEntry                    -- ^ misc rts entry points,     suffix _entry
  | CmmRetInfo                  -- ^ misc rts ret info tables,  suffix _info
  | CmmRet                      -- ^ misc rts return points,    suffix _ret
  | CmmData                     -- ^ misc rts data bits, eg CHARLIKE_closure
  | CmmCode                     -- ^ misc rts code
  | CmmClosure                  -- ^ closures eg CHARLIKE_closure
  | CmmPrimCall                 -- ^ a prim call to some hand written Cmm code
  deriving (Eq, Ord)

data DynamicLinkerLabelInfo
  = CodeStub                    -- MachO: Lfoo$stub, ELF: foo@plt
  | SymbolPtr                   -- MachO: Lfoo$non_lazy_ptr, Windows: __imp_foo
  | GotSymbolPtr                -- ELF: foo@got
  | GotSymbolOffset             -- ELF: foo@gotoff

  deriving (Eq, Ord)


-- -----------------------------------------------------------------------------
-- Constructing CLabels
-- -----------------------------------------------------------------------------

-- Constructing IdLabels
-- These are always local:

mkSRTLabel     :: Unique -> CLabel
mkSRTLabel u = SRTLabel u

mkRednCountsLabel :: Name -> CLabel
mkRednCountsLabel name = IdLabel name NoCafRefs RednCounts  -- Note [ticky for LNE]

-- These have local & (possibly) external variants:
mkLocalClosureLabel      :: Name -> CafInfo -> CLabel
mkLocalInfoTableLabel    :: Name -> CafInfo -> CLabel
mkLocalClosureTableLabel :: Name -> CafInfo -> CLabel
mkLocalClosureLabel   !name !c  = IdLabel name  c Closure
mkLocalInfoTableLabel   name c  = IdLabel name  c LocalInfoTable
mkLocalClosureTableLabel name c = IdLabel name  c ClosureTable

mkClosureLabel              :: Name -> CafInfo -> CLabel
mkInfoTableLabel            :: Name -> CafInfo -> CLabel
mkEntryLabel                :: Name -> CafInfo -> CLabel
mkClosureTableLabel         :: Name -> CafInfo -> CLabel
mkConInfoTableLabel         :: Name -> CafInfo -> CLabel
mkBytesLabel                :: Name -> CLabel
mkClosureLabel name         c     = IdLabel name c Closure
mkInfoTableLabel name       c     = IdLabel name c InfoTable
mkEntryLabel name           c     = IdLabel name c Entry
mkClosureTableLabel name    c     = IdLabel name c ClosureTable
mkConInfoTableLabel name    c     = IdLabel name c ConInfoTable
mkBytesLabel name                 = IdLabel name NoCafRefs Bytes

mkBlockInfoTableLabel :: Name -> CafInfo -> CLabel
mkBlockInfoTableLabel name c = IdLabel name c BlockInfoTable
                               -- See Note [Proc-point local block entry-point].

-- Constructing Cmm Labels
mkDirty_MUT_VAR_Label,
    mkNonmovingWriteBarrierEnabledLabel,
    mkUpdInfoLabel,
    mkBHUpdInfoLabel, mkIndStaticInfoLabel, mkMainCapabilityLabel,
    mkMAP_FROZEN_CLEAN_infoLabel, mkMAP_FROZEN_DIRTY_infoLabel,
    mkMAP_DIRTY_infoLabel,
    mkArrWords_infoLabel,
    mkTopTickyCtrLabel,
    mkCAFBlackHoleInfoTableLabel,
    mkSMAP_FROZEN_CLEAN_infoLabel, mkSMAP_FROZEN_DIRTY_infoLabel,
    mkSMAP_DIRTY_infoLabel, mkBadAlignmentLabel :: CLabel
mkDirty_MUT_VAR_Label           = mkForeignLabel (fsLit "dirty_MUT_VAR") Nothing ForeignLabelInExternalPackage IsFunction
mkNonmovingWriteBarrierEnabledLabel
                                = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "nonmoving_write_barrier_enabled") CmmData
mkUpdInfoLabel                  = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_upd_frame")         CmmInfo
mkBHUpdInfoLabel                = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_bh_upd_frame" )     CmmInfo
mkIndStaticInfoLabel            = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_IND_STATIC")        CmmInfo
mkMainCapabilityLabel           = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "MainCapability")        CmmData
mkMAP_FROZEN_CLEAN_infoLabel    = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_MUT_ARR_PTRS_FROZEN_CLEAN") CmmInfo
mkMAP_FROZEN_DIRTY_infoLabel    = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_MUT_ARR_PTRS_FROZEN_DIRTY") CmmInfo
mkMAP_DIRTY_infoLabel           = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_MUT_ARR_PTRS_DIRTY") CmmInfo
mkTopTickyCtrLabel              = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "top_ct")                CmmData
mkCAFBlackHoleInfoTableLabel    = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_CAF_BLACKHOLE")     CmmInfo
mkArrWords_infoLabel            = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_ARR_WORDS")         CmmInfo
mkSMAP_FROZEN_CLEAN_infoLabel   = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN") CmmInfo
mkSMAP_FROZEN_DIRTY_infoLabel   = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY") CmmInfo
mkSMAP_DIRTY_infoLabel          = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_SMALL_MUT_ARR_PTRS_DIRTY") CmmInfo
mkBadAlignmentLabel             = CmmLabel rtsUnitId (NeedExternDecl False) (fsLit "stg_badAlignment")      CmmEntry

mkSRTInfoLabel :: Int -> CLabel
mkSRTInfoLabel n = CmmLabel rtsUnitId (NeedExternDecl False) lbl CmmInfo
 where
   lbl =
     case n of
       1 -> fsLit "stg_SRT_1"
       2 -> fsLit "stg_SRT_2"
       3 -> fsLit "stg_SRT_3"
       4 -> fsLit "stg_SRT_4"
       5 -> fsLit "stg_SRT_5"
       6 -> fsLit "stg_SRT_6"
       7 -> fsLit "stg_SRT_7"
       8 -> fsLit "stg_SRT_8"
       9 -> fsLit "stg_SRT_9"
       10 -> fsLit "stg_SRT_10"
       11 -> fsLit "stg_SRT_11"
       12 -> fsLit "stg_SRT_12"
       13 -> fsLit "stg_SRT_13"
       14 -> fsLit "stg_SRT_14"
       15 -> fsLit "stg_SRT_15"
       16 -> fsLit "stg_SRT_16"
       _ -> panic "mkSRTInfoLabel"

-----
mkCmmInfoLabel,   mkCmmEntryLabel, mkCmmRetInfoLabel, mkCmmRetLabel,
  mkCmmCodeLabel, mkCmmClosureLabel
        :: UnitId -> FastString -> CLabel

mkCmmDataLabel    :: UnitId -> NeedExternDecl -> FastString -> CLabel
mkRtsCmmDataLabel :: FastString -> CLabel

mkCmmInfoLabel       pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmInfo
mkCmmEntryLabel      pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmEntry
mkCmmRetInfoLabel    pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmRetInfo
mkCmmRetLabel        pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmRet
mkCmmCodeLabel       pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmCode
mkCmmClosureLabel    pkg str     = CmmLabel pkg (NeedExternDecl True) str CmmClosure
mkCmmDataLabel       pkg ext str = CmmLabel pkg ext  str CmmData
mkRtsCmmDataLabel    str         = CmmLabel rtsUnitId (NeedExternDecl False)  str CmmData
                                    -- RTS symbols don't need "GHC.CmmToC" to
                                    -- generate \"extern\" declaration (they are
                                    -- exposed via includes/Stg.h)

mkLocalBlockLabel :: Unique -> CLabel
mkLocalBlockLabel u = LocalBlockLabel u

-- Constructing RtsLabels
mkRtsPrimOpLabel :: PrimOp -> CLabel
mkRtsPrimOpLabel primop = RtsLabel (RtsPrimOp primop)

mkSelectorInfoLabel :: DynFlags -> Bool -> Int -> CLabel
mkSelectorInfoLabel dflags upd offset =
   ASSERT(offset >= 0 && offset <= mAX_SPEC_SELECTEE_SIZE dflags)
   RtsLabel (RtsSelectorInfoTable upd offset)

mkSelectorEntryLabel :: DynFlags -> Bool -> Int -> CLabel
mkSelectorEntryLabel dflags upd offset =
   ASSERT(offset >= 0 && offset <= mAX_SPEC_SELECTEE_SIZE dflags)
   RtsLabel (RtsSelectorEntry upd offset)

mkApInfoTableLabel :: DynFlags -> Bool -> Int -> CLabel
mkApInfoTableLabel dflags upd arity =
   ASSERT(arity > 0 && arity <= mAX_SPEC_AP_SIZE dflags)
   RtsLabel (RtsApInfoTable upd arity)

mkApEntryLabel :: DynFlags -> Bool -> Int -> CLabel
mkApEntryLabel dflags upd arity =
   ASSERT(arity > 0 && arity <= mAX_SPEC_AP_SIZE dflags)
   RtsLabel (RtsApEntry upd arity)


-- A call to some primitive hand written Cmm code
mkPrimCallLabel :: PrimCall -> CLabel
mkPrimCallLabel (PrimCall str pkg)
        = CmmLabel (toUnitId pkg) (NeedExternDecl True) str CmmPrimCall


-- Constructing ForeignLabels

-- | Make a foreign label
mkForeignLabel
        :: FastString           -- name
        -> Maybe Int            -- size prefix
        -> ForeignLabelSource   -- what package it's in
        -> FunctionOrData
        -> CLabel

mkForeignLabel = ForeignLabel


-- | Update the label size field in a ForeignLabel
addLabelSize :: CLabel -> Int -> CLabel
addLabelSize (ForeignLabel str _ src  fod) sz
    = ForeignLabel str (Just sz) src fod
addLabelSize label _
    = label

-- | Whether label is a top-level string literal
isBytesLabel :: CLabel -> Bool
isBytesLabel (IdLabel _ _ Bytes) = True
isBytesLabel _lbl = False

-- | Whether label is a non-haskell label (defined in C code)
isForeignLabel :: CLabel -> Bool
isForeignLabel (ForeignLabel _ _ _ _) = True
isForeignLabel _lbl = False

-- | Whether label is a static closure label (can come from haskell or cmm)
isStaticClosureLabel :: CLabel -> Bool
-- Closure defined in haskell (.hs)
isStaticClosureLabel (IdLabel _ _ Closure) = True
-- Closure defined in cmm
isStaticClosureLabel (CmmLabel _ _ _ CmmClosure) = True
isStaticClosureLabel _lbl = False

-- | Whether label is a .rodata label
isSomeRODataLabel :: CLabel -> Bool
-- info table defined in haskell (.hs)
isSomeRODataLabel (IdLabel _ _ ClosureTable) = True
isSomeRODataLabel (IdLabel _ _ ConInfoTable) = True
isSomeRODataLabel (IdLabel _ _ InfoTable) = True
isSomeRODataLabel (IdLabel _ _ LocalInfoTable) = True
isSomeRODataLabel (IdLabel _ _ BlockInfoTable) = True
-- info table defined in cmm (.cmm)
isSomeRODataLabel (CmmLabel _ _ _ CmmInfo) = True
isSomeRODataLabel _lbl = False

-- | Whether label is points to some kind of info table
isInfoTableLabel :: CLabel -> Bool
isInfoTableLabel (IdLabel _ _ InfoTable)      = True
isInfoTableLabel (IdLabel _ _ LocalInfoTable) = True
isInfoTableLabel (IdLabel _ _ ConInfoTable)   = True
isInfoTableLabel (IdLabel _ _ BlockInfoTable) = True
isInfoTableLabel _                            = False

-- | Whether label is points to constructor info table
isConInfoTableLabel :: CLabel -> Bool
isConInfoTableLabel (IdLabel _ _ ConInfoTable)   = True
isConInfoTableLabel _                            = False

-- | Get the label size field from a ForeignLabel
foreignLabelStdcallInfo :: CLabel -> Maybe Int
foreignLabelStdcallInfo (ForeignLabel _ info _ _) = info
foreignLabelStdcallInfo _lbl = Nothing


-- Constructing Large*Labels
mkBitmapLabel   :: Unique -> CLabel
mkBitmapLabel   uniq            = LargeBitmapLabel uniq

-- Constructing Cost Center Labels
mkCCLabel  :: CostCentre      -> CLabel
mkCCSLabel :: CostCentreStack -> CLabel
mkCCLabel           cc          = CC_Label cc
mkCCSLabel          ccs         = CCS_Label ccs

mkRtsApFastLabel :: FastString -> CLabel
mkRtsApFastLabel str = RtsLabel (RtsApFast str)

mkRtsSlowFastTickyCtrLabel :: String -> CLabel
mkRtsSlowFastTickyCtrLabel pat = RtsLabel (RtsSlowFastTickyCtr pat)


-- Constructing Code Coverage Labels
mkHpcTicksLabel :: Module -> CLabel
mkHpcTicksLabel                = HpcTicksLabel


-- Constructing labels used for dynamic linking
mkDynamicLinkerLabel :: DynamicLinkerLabelInfo -> CLabel -> CLabel
mkDynamicLinkerLabel            = DynamicLinkerLabel

dynamicLinkerLabelInfo :: CLabel -> Maybe (DynamicLinkerLabelInfo, CLabel)
dynamicLinkerLabelInfo (DynamicLinkerLabel info lbl) = Just (info, lbl)
dynamicLinkerLabelInfo _        = Nothing

mkPicBaseLabel :: CLabel
mkPicBaseLabel                  = PicBaseLabel


-- Constructing miscellaneous other labels
mkDeadStripPreventer :: CLabel -> CLabel
mkDeadStripPreventer lbl        = DeadStripPreventer lbl

mkStringLitLabel :: Unique -> CLabel
mkStringLitLabel                = StringLitLabel

mkAsmTempLabel :: Uniquable a => a -> CLabel
mkAsmTempLabel a                = AsmTempLabel (getUnique a)

mkAsmTempDerivedLabel :: CLabel -> FastString -> CLabel
mkAsmTempDerivedLabel = AsmTempDerivedLabel

mkAsmTempEndLabel :: CLabel -> CLabel
mkAsmTempEndLabel l = mkAsmTempDerivedLabel l (fsLit "_end")

-- | Construct a label for a DWARF Debug Information Entity (DIE)
-- describing another symbol.
mkAsmTempDieLabel :: CLabel -> CLabel
mkAsmTempDieLabel l = mkAsmTempDerivedLabel l (fsLit "_die")

-- -----------------------------------------------------------------------------
-- Convert between different kinds of label

toClosureLbl :: CLabel -> CLabel
toClosureLbl (IdLabel n c _) = IdLabel n c Closure
toClosureLbl (CmmLabel m ext str _) = CmmLabel m ext str CmmClosure
toClosureLbl l = pprPanic "toClosureLbl" (ppr l)

toSlowEntryLbl :: CLabel -> CLabel
toSlowEntryLbl (IdLabel n _ BlockInfoTable)
  = pprPanic "toSlowEntryLbl" (ppr n)
toSlowEntryLbl (IdLabel n c _) = IdLabel n c Slow
toSlowEntryLbl l = pprPanic "toSlowEntryLbl" (ppr l)

toEntryLbl :: CLabel -> CLabel
toEntryLbl (IdLabel n c LocalInfoTable)  = IdLabel n c LocalEntry
toEntryLbl (IdLabel n c ConInfoTable)    = IdLabel n c ConEntry
toEntryLbl (IdLabel n _ BlockInfoTable)  = mkLocalBlockLabel (nameUnique n)
                              -- See Note [Proc-point local block entry-point].
toEntryLbl (IdLabel n c _)               = IdLabel n c Entry
toEntryLbl (CmmLabel m ext str CmmInfo)    = CmmLabel m ext str CmmEntry
toEntryLbl (CmmLabel m ext str CmmRetInfo) = CmmLabel m ext str CmmRet
toEntryLbl l = pprPanic "toEntryLbl" (ppr l)

toInfoLbl :: CLabel -> CLabel
toInfoLbl (IdLabel n c LocalEntry)     = IdLabel n c LocalInfoTable
toInfoLbl (IdLabel n c ConEntry)       = IdLabel n c ConInfoTable
toInfoLbl (IdLabel n c _)              = IdLabel n c InfoTable
toInfoLbl (CmmLabel m ext str CmmEntry)= CmmLabel m ext str CmmInfo
toInfoLbl (CmmLabel m ext str CmmRet)  = CmmLabel m ext str CmmRetInfo
toInfoLbl l = pprPanic "CLabel.toInfoLbl" (ppr l)

hasHaskellName :: CLabel -> Maybe Name
hasHaskellName (IdLabel n _ _) = Just n
hasHaskellName _               = Nothing

-- -----------------------------------------------------------------------------
-- Does a CLabel's referent itself refer to a CAF?
hasCAF :: CLabel -> Bool
hasCAF (IdLabel _ _ RednCounts) = False -- Note [ticky for LNE]
hasCAF (IdLabel _ MayHaveCafRefs _) = True
hasCAF _                            = False

-- Note [ticky for LNE]
-- ~~~~~~~~~~~~~~~~~~~~~

-- Until 14 Feb 2013, every ticky counter was associated with a
-- closure. Thus, ticky labels used IdLabel. It is odd that
-- GHC.Cmm.Info.Build.cafTransfers would consider such a ticky label
-- reason to add the name to the CAFEnv (and thus eventually the SRT),
-- but it was harmless because the ticky was only used if the closure
-- was also.
--
-- Since we now have ticky counters for LNEs, it is no longer the case
-- that every ticky counter has an actual closure. So I changed the
-- generation of ticky counters' CLabels to not result in their
-- associated id ending up in the SRT.
--
-- NB IdLabel is still appropriate for ticky ids (as opposed to
-- CmmLabel) because the LNE's counter is still related to an .hs Id,
-- that Id just isn't for a proper closure.

-- -----------------------------------------------------------------------------
-- Does a CLabel need declaring before use or not?
--
-- See wiki:commentary/compiler/backends/ppr-c#prototypes

needsCDecl :: CLabel -> Bool
  -- False <=> it's pre-declared; don't bother
  -- don't bother declaring Bitmap labels, we always make sure
  -- they are defined before use.
needsCDecl (SRTLabel _)                 = True
needsCDecl (LargeBitmapLabel _)         = False
needsCDecl (IdLabel _ _ _)              = True
needsCDecl (LocalBlockLabel _)          = True

needsCDecl (StringLitLabel _)           = False
needsCDecl (AsmTempLabel _)             = False
needsCDecl (AsmTempDerivedLabel _ _)    = False
needsCDecl (RtsLabel _)                 = False

needsCDecl (CmmLabel pkgId (NeedExternDecl external) _ _)
        -- local labels mustn't have it
        | not external                  = False

        -- Prototypes for labels defined in the runtime system are imported
        --      into HC files via includes/Stg.h.
        | pkgId == rtsUnitId            = False

        -- For other labels we inline one into the HC file directly.
        | otherwise                     = True

needsCDecl l@(ForeignLabel{})           = not (isMathFun l)
needsCDecl (CC_Label _)                 = True
needsCDecl (CCS_Label _)                = True
needsCDecl (HpcTicksLabel _)            = True
needsCDecl (DynamicLinkerLabel {})      = panic "needsCDecl DynamicLinkerLabel"
needsCDecl PicBaseLabel                 = panic "needsCDecl PicBaseLabel"
needsCDecl (DeadStripPreventer {})      = panic "needsCDecl DeadStripPreventer"

-- | If a label is a local block label then return just its 'BlockId', otherwise
-- 'Nothing'.
maybeLocalBlockLabel :: CLabel -> Maybe BlockId
maybeLocalBlockLabel (LocalBlockLabel uq)  = Just $ mkBlockId uq
maybeLocalBlockLabel _                     = Nothing


-- | Check whether a label corresponds to a C function that has
--      a prototype in a system header somewhere, or is built-in
--      to the C compiler. For these labels we avoid generating our
--      own C prototypes.
isMathFun :: CLabel -> Bool
isMathFun (ForeignLabel fs _ _ _)       = fs `elementOfUniqSet` math_funs
isMathFun _ = False

math_funs :: UniqSet FastString
math_funs = mkUniqSet [
        -- _ISOC99_SOURCE
        (fsLit "acos"),         (fsLit "acosf"),        (fsLit "acosh"),
        (fsLit "acoshf"),       (fsLit "acoshl"),       (fsLit "acosl"),
        (fsLit "asin"),         (fsLit "asinf"),        (fsLit "asinl"),
        (fsLit "asinh"),        (fsLit "asinhf"),       (fsLit "asinhl"),
        (fsLit "atan"),         (fsLit "atanf"),        (fsLit "atanl"),
        (fsLit "atan2"),        (fsLit "atan2f"),       (fsLit "atan2l"),
        (fsLit "atanh"),        (fsLit "atanhf"),       (fsLit "atanhl"),
        (fsLit "cbrt"),         (fsLit "cbrtf"),        (fsLit "cbrtl"),
        (fsLit "ceil"),         (fsLit "ceilf"),        (fsLit "ceill"),
        (fsLit "copysign"),     (fsLit "copysignf"),    (fsLit "copysignl"),
        (fsLit "cos"),          (fsLit "cosf"),         (fsLit "cosl"),
        (fsLit "cosh"),         (fsLit "coshf"),        (fsLit "coshl"),
        (fsLit "erf"),          (fsLit "erff"),         (fsLit "erfl"),
        (fsLit "erfc"),         (fsLit "erfcf"),        (fsLit "erfcl"),
        (fsLit "exp"),          (fsLit "expf"),         (fsLit "expl"),
        (fsLit "exp2"),         (fsLit "exp2f"),        (fsLit "exp2l"),
        (fsLit "expm1"),        (fsLit "expm1f"),       (fsLit "expm1l"),
        (fsLit "fabs"),         (fsLit "fabsf"),        (fsLit "fabsl"),
        (fsLit "fdim"),         (fsLit "fdimf"),        (fsLit "fdiml"),
        (fsLit "floor"),        (fsLit "floorf"),       (fsLit "floorl"),
        (fsLit "fma"),          (fsLit "fmaf"),         (fsLit "fmal"),
        (fsLit "fmax"),         (fsLit "fmaxf"),        (fsLit "fmaxl"),
        (fsLit "fmin"),         (fsLit "fminf"),        (fsLit "fminl"),
        (fsLit "fmod"),         (fsLit "fmodf"),        (fsLit "fmodl"),
        (fsLit "frexp"),        (fsLit "frexpf"),       (fsLit "frexpl"),
        (fsLit "hypot"),        (fsLit "hypotf"),       (fsLit "hypotl"),
        (fsLit "ilogb"),        (fsLit "ilogbf"),       (fsLit "ilogbl"),
        (fsLit "ldexp"),        (fsLit "ldexpf"),       (fsLit "ldexpl"),
        (fsLit "lgamma"),       (fsLit "lgammaf"),      (fsLit "lgammal"),
        (fsLit "llrint"),       (fsLit "llrintf"),      (fsLit "llrintl"),
        (fsLit "llround"),      (fsLit "llroundf"),     (fsLit "llroundl"),
        (fsLit "log"),          (fsLit "logf"),         (fsLit "logl"),
        (fsLit "log10l"),       (fsLit "log10"),        (fsLit "log10f"),
        (fsLit "log1pl"),       (fsLit "log1p"),        (fsLit "log1pf"),
        (fsLit "log2"),         (fsLit "log2f"),        (fsLit "log2l"),
        (fsLit "logb"),         (fsLit "logbf"),        (fsLit "logbl"),
        (fsLit "lrint"),        (fsLit "lrintf"),       (fsLit "lrintl"),
        (fsLit "lround"),       (fsLit "lroundf"),      (fsLit "lroundl"),
        (fsLit "modf"),         (fsLit "modff"),        (fsLit "modfl"),
        (fsLit "nan"),          (fsLit "nanf"),         (fsLit "nanl"),
        (fsLit "nearbyint"),    (fsLit "nearbyintf"),   (fsLit "nearbyintl"),
        (fsLit "nextafter"),    (fsLit "nextafterf"),   (fsLit "nextafterl"),
        (fsLit "nexttoward"),   (fsLit "nexttowardf"),  (fsLit "nexttowardl"),
        (fsLit "pow"),          (fsLit "powf"),         (fsLit "powl"),
        (fsLit "remainder"),    (fsLit "remainderf"),   (fsLit "remainderl"),
        (fsLit "remquo"),       (fsLit "remquof"),      (fsLit "remquol"),
        (fsLit "rint"),         (fsLit "rintf"),        (fsLit "rintl"),
        (fsLit "round"),        (fsLit "roundf"),       (fsLit "roundl"),
        (fsLit "scalbln"),      (fsLit "scalblnf"),     (fsLit "scalblnl"),
        (fsLit "scalbn"),       (fsLit "scalbnf"),      (fsLit "scalbnl"),
        (fsLit "sin"),          (fsLit "sinf"),         (fsLit "sinl"),
        (fsLit "sinh"),         (fsLit "sinhf"),        (fsLit "sinhl"),
        (fsLit "sqrt"),         (fsLit "sqrtf"),        (fsLit "sqrtl"),
        (fsLit "tan"),          (fsLit "tanf"),         (fsLit "tanl"),
        (fsLit "tanh"),         (fsLit "tanhf"),        (fsLit "tanhl"),
        (fsLit "tgamma"),       (fsLit "tgammaf"),      (fsLit "tgammal"),
        (fsLit "trunc"),        (fsLit "truncf"),       (fsLit "truncl"),
        -- ISO C 99 also defines these function-like macros in math.h:
        -- fpclassify, isfinite, isinf, isnormal, signbit, isgreater,
        -- isgreaterequal, isless, islessequal, islessgreater, isunordered

        -- additional symbols from _BSD_SOURCE
        (fsLit "drem"),         (fsLit "dremf"),        (fsLit "dreml"),
        (fsLit "finite"),       (fsLit "finitef"),      (fsLit "finitel"),
        (fsLit "gamma"),        (fsLit "gammaf"),       (fsLit "gammal"),
        (fsLit "isinf"),        (fsLit "isinff"),       (fsLit "isinfl"),
        (fsLit "isnan"),        (fsLit "isnanf"),       (fsLit "isnanl"),
        (fsLit "j0"),           (fsLit "j0f"),          (fsLit "j0l"),
        (fsLit "j1"),           (fsLit "j1f"),          (fsLit "j1l"),
        (fsLit "jn"),           (fsLit "jnf"),          (fsLit "jnl"),
        (fsLit "lgamma_r"),     (fsLit "lgammaf_r"),    (fsLit "lgammal_r"),
        (fsLit "scalb"),        (fsLit "scalbf"),       (fsLit "scalbl"),
        (fsLit "significand"),  (fsLit "significandf"), (fsLit "significandl"),
        (fsLit "y0"),           (fsLit "y0f"),          (fsLit "y0l"),
        (fsLit "y1"),           (fsLit "y1f"),          (fsLit "y1l"),
        (fsLit "yn"),           (fsLit "ynf"),          (fsLit "ynl"),

        -- These functions are described in IEEE Std 754-2008 -
        -- Standard for Floating-Point Arithmetic and ISO/IEC TS 18661
        (fsLit "nextup"),       (fsLit "nextupf"),      (fsLit "nextupl"),
        (fsLit "nextdown"),     (fsLit "nextdownf"),    (fsLit "nextdownl")
    ]

-- -----------------------------------------------------------------------------
-- | Is a CLabel visible outside this object file or not?
--      From the point of view of the code generator, a name is
--      externally visible if it has to be declared as exported
--      in the .o file's symbol table; that is, made non-static.
externallyVisibleCLabel :: CLabel -> Bool -- not C "static"
externallyVisibleCLabel (StringLitLabel _)      = False
externallyVisibleCLabel (AsmTempLabel _)        = False
externallyVisibleCLabel (AsmTempDerivedLabel _ _)= False
externallyVisibleCLabel (RtsLabel _)            = True
externallyVisibleCLabel (LocalBlockLabel _)     = False
externallyVisibleCLabel (CmmLabel _ _ _ _)      = True
externallyVisibleCLabel (ForeignLabel{})        = True
externallyVisibleCLabel (IdLabel name _ info)   = isExternalName name && externallyVisibleIdLabel info
externallyVisibleCLabel (CC_Label _)            = True
externallyVisibleCLabel (CCS_Label _)           = True
externallyVisibleCLabel (DynamicLinkerLabel _ _)  = False
externallyVisibleCLabel (HpcTicksLabel _)       = True
externallyVisibleCLabel (LargeBitmapLabel _)    = False
externallyVisibleCLabel (SRTLabel _)            = False
externallyVisibleCLabel (PicBaseLabel {}) = panic "externallyVisibleCLabel PicBaseLabel"
externallyVisibleCLabel (DeadStripPreventer {}) = panic "externallyVisibleCLabel DeadStripPreventer"

externallyVisibleIdLabel :: IdLabelInfo -> Bool
externallyVisibleIdLabel LocalInfoTable  = False
externallyVisibleIdLabel LocalEntry      = False
externallyVisibleIdLabel BlockInfoTable  = False
externallyVisibleIdLabel _               = True

-- -----------------------------------------------------------------------------
-- Finding the "type" of a CLabel

-- For generating correct types in label declarations:

data CLabelType
  = CodeLabel   -- Address of some executable instructions
  | DataLabel   -- Address of data, not a GC ptr
  | GcPtrLabel  -- Address of a (presumably static) GC object

isCFunctionLabel :: CLabel -> Bool
isCFunctionLabel lbl = case labelType lbl of
                        CodeLabel -> True
                        _other    -> False

isGcPtrLabel :: CLabel -> Bool
isGcPtrLabel lbl = case labelType lbl of
                        GcPtrLabel -> True
                        _other     -> False


-- | Work out the general type of data at the address of this label
--    whether it be code, data, or static GC object.
labelType :: CLabel -> CLabelType
labelType (IdLabel _ _ info)                    = idInfoLabelType info
labelType (CmmLabel _ _ _ CmmData)              = DataLabel
labelType (CmmLabel _ _ _ CmmClosure)           = GcPtrLabel
labelType (CmmLabel _ _ _ CmmCode)              = CodeLabel
labelType (CmmLabel _ _ _ CmmInfo)              = DataLabel
labelType (CmmLabel _ _ _ CmmEntry)             = CodeLabel
labelType (CmmLabel _ _ _ CmmPrimCall)          = CodeLabel
labelType (CmmLabel _ _ _ CmmRetInfo)           = DataLabel
labelType (CmmLabel _ _ _ CmmRet)               = CodeLabel
labelType (RtsLabel (RtsSelectorInfoTable _ _)) = DataLabel
labelType (RtsLabel (RtsApInfoTable _ _))       = DataLabel
labelType (RtsLabel (RtsApFast _))              = CodeLabel
labelType (RtsLabel _)                          = DataLabel
labelType (LocalBlockLabel _)                   = CodeLabel
labelType (SRTLabel _)                          = DataLabel
labelType (ForeignLabel _ _ _ IsFunction)       = CodeLabel
labelType (ForeignLabel _ _ _ IsData)           = DataLabel
labelType (AsmTempLabel _)                      = panic "labelType(AsmTempLabel)"
labelType (AsmTempDerivedLabel _ _)             = panic "labelType(AsmTempDerivedLabel)"
labelType (StringLitLabel _)                    = DataLabel
labelType (CC_Label _)                          = DataLabel
labelType (CCS_Label _)                         = DataLabel
labelType (DynamicLinkerLabel _ _)              = DataLabel -- Is this right?
labelType PicBaseLabel                          = DataLabel
labelType (DeadStripPreventer _)                = DataLabel
labelType (HpcTicksLabel _)                     = DataLabel
labelType (LargeBitmapLabel _)                  = DataLabel

idInfoLabelType :: IdLabelInfo -> CLabelType
idInfoLabelType info =
  case info of
    InfoTable     -> DataLabel
    LocalInfoTable -> DataLabel
    BlockInfoTable -> DataLabel
    Closure       -> GcPtrLabel
    ConInfoTable  -> DataLabel
    ClosureTable  -> DataLabel
    RednCounts    -> DataLabel
    Bytes         -> DataLabel
    _             -> CodeLabel


-- -----------------------------------------------------------------------------

-- | Is a 'CLabel' defined in the current module being compiled?
--
-- Sometimes we can optimise references within a compilation unit in ways that
-- we couldn't for inter-module references. This provides a conservative
-- estimate of whether a 'CLabel' lives in the current module.
isLocalCLabel :: Module -> CLabel -> Bool
isLocalCLabel this_mod lbl =
  case lbl of
    IdLabel name _ _
      | isInternalName name -> True
      | otherwise           -> nameModule name == this_mod
    LocalBlockLabel _       -> True
    _                       -> False

-- -----------------------------------------------------------------------------

-- | Does a 'CLabel' need dynamic linkage?
--
-- When referring to data in code, we need to know whether
-- that data resides in a DLL or not. [Win32 only.]
-- @labelDynamic@ returns @True@ if the label is located
-- in a DLL, be it a data reference or not.
labelDynamic :: NCGConfig -> Module -> CLabel -> Bool
labelDynamic config this_mod lbl =
  case lbl of
   -- is the RTS in a DLL or not?
   RtsLabel _ ->
     externalDynamicRefs && (this_pkg /= rtsUnit)

   IdLabel n _ _ ->
     externalDynamicRefs && isDynLinkName platform this_mod n

   -- When compiling in the "dyn" way, each package is to be linked into
   -- its own shared library.
   CmmLabel pkg _ _ _
    | os == OSMinGW32 -> externalDynamicRefs && (toUnitId this_pkg /= pkg)
    | otherwise       -> externalDynamicRefs

   LocalBlockLabel _    -> False

   ForeignLabel _ _ source _  ->
       if os == OSMinGW32
       then case source of
            -- Foreign label is in some un-named foreign package (or DLL).
            ForeignLabelInExternalPackage -> True

            -- Foreign label is linked into the same package as the
            -- source file currently being compiled.
            ForeignLabelInThisPackage -> False

            -- Foreign label is in some named package.
            -- When compiling in the "dyn" way, each package is to be
            -- linked into its own DLL.
            ForeignLabelInPackage pkgId ->
                externalDynamicRefs && (this_pkg /= pkgId)

       else -- On Mac OS X and on ELF platforms, false positives are OK,
            -- so we claim that all foreign imports come from dynamic
            -- libraries
            True

   CC_Label cc ->
     externalDynamicRefs && not (ccFromThisModule cc this_mod)

   -- CCS_Label always contains a CostCentre defined in the current module
   CCS_Label _ -> False

   HpcTicksLabel m ->
     externalDynamicRefs && this_mod /= m

   -- Note that DynamicLinkerLabels do NOT require dynamic linking themselves.
   _                 -> False
  where
    externalDynamicRefs = ncgExternalDynamicRefs config
    platform = ncgPlatform config
    os = platformOS platform
    this_pkg = moduleUnit this_mod


-----------------------------------------------------------------------------
-- Printing out CLabels.

{-
Convention:

      <name>_<type>

where <name> is <Module>_<name> for external names and <unique> for
internal names. <type> is one of the following:

         info                   Info table
         srt                    Static reference table
         entry                  Entry code (function, closure)
         slow                   Slow entry code (if any)
         ret                    Direct return address
         vtbl                   Vector table
         <n>_alt                Case alternative (tag n)
         dflt                   Default case alternative
         btm                    Large bitmap vector
         closure                Static closure
         con_entry              Dynamic Constructor entry code
         con_info               Dynamic Constructor info table
         static_entry           Static Constructor entry code
         static_info            Static Constructor info table
         sel_info               Selector info table
         sel_entry              Selector entry code
         cc                     Cost centre
         ccs                    Cost centre stack

Many of these distinctions are only for documentation reasons.  For
example, _ret is only distinguished from _entry to make it easy to
tell whether a code fragment is a return point or a closure/function
entry.

Note [Closure and info labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a function 'foo, we have:
   foo_info    : Points to the info table describing foo's closure
                 (and entry code for foo with tables next to code)
   foo_closure : Static (no-free-var) closure only:
                 points to the statically-allocated closure

For a data constructor (such as Just or Nothing), we have:
    Just_con_info: Info table for the data constructor itself
                   the first word of a heap-allocated Just
    Just_info:     Info table for the *worker function*, an
                   ordinary Haskell function of arity 1 that
                   allocates a (Just x) box:
                      Just = \x -> Just x
    Just_closure:  The closure for this worker

    Nothing_closure: a statically allocated closure for Nothing
    Nothing_static_info: info table for Nothing_closure

All these must be exported symbol, EXCEPT Just_info.  We don't need to
export this because in other modules we either have
       * A reference to 'Just'; use Just_closure
       * A saturated call 'Just x'; allocate using Just_con_info
Not exporting these Just_info labels reduces the number of symbols
somewhat.

Note [Bytes label]
~~~~~~~~~~~~~~~~~~
For a top-level string literal 'foo', we have just one symbol 'foo_bytes', which
points to a static data block containing the content of the literal.

Note [Proc-point local block entry-points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A label for a proc-point local block entry-point has no "_entry" suffix. With
`infoTblLbl` we derive an info table label from a proc-point block ID. If
we convert such an info table label into an entry label we must produce
the label without an "_entry" suffix. So an info table label records
the fact that it was derived from a block ID in `IdLabelInfo` as
`BlockInfoTable`.

The info table label and the local block label are both local labels
and are not externally visible.
-}

instance Outputable CLabel where
  ppr c = sdocWithDynFlags $ \dynFlags -> pprCLabel dynFlags c

pprCLabel :: DynFlags -> CLabel -> SDoc
pprCLabel dflags = \case
   (LocalBlockLabel u) -> tempLabelPrefixOrUnderscore platform <> pprUniqueAlways u

   (AsmTempLabel u)
      | not (platformUnregisterised platform)
      -> tempLabelPrefixOrUnderscore platform <> pprUniqueAlways u

   (AsmTempDerivedLabel l suf)
      | useNCG
      -> ptext (asmTempLabelPrefix platform)
         <> case l of AsmTempLabel u    -> pprUniqueAlways u
                      LocalBlockLabel u -> pprUniqueAlways u
                      _other            -> pprCLabel dflags l
         <> ftext suf

   (DynamicLinkerLabel info lbl)
      | useNCG
      -> pprDynamicLinkerAsmLabel platform info lbl

   PicBaseLabel
      | useNCG
      -> text "1b"

   (DeadStripPreventer lbl)
      | useNCG
      ->
      {-
         `lbl` can be temp one but we need to ensure that dsp label will stay
         in the final binary so we prepend non-temp prefix ("dsp_") and
         optional `_` (underscore) because this is how you mark non-temp symbols
         on some platforms (Darwin)
      -}
      maybe_underscore $ text "dsp_" <> pprCLabel dflags lbl <> text "_dsp"

   (StringLitLabel u)
      | useNCG
      -> pprUniqueAlways u <> ptext (sLit "_str")

   lbl -> getPprStyle $ \sty ->
            if useNCG && asmStyle sty
            then maybe_underscore $ pprAsmCLbl lbl
            else pprCLbl platform lbl

  where
    platform = targetPlatform dflags
    useNCG   = hscTarget dflags == HscAsm

    maybe_underscore :: SDoc -> SDoc
    maybe_underscore doc =
      if platformLeadingUnderscore platform
      then pp_cSEP <> doc
      else doc

    pprAsmCLbl (ForeignLabel fs (Just sz) _ _)
     | platformOS platform == OSMinGW32
        -- In asm mode, we need to put the suffix on a stdcall ForeignLabel.
        -- (The C compiler does this itself).
        = ftext fs <> char '@' <> int sz
    pprAsmCLbl lbl = pprCLbl platform lbl

pprCLbl :: Platform -> CLabel -> SDoc
pprCLbl platform = \case
   (StringLitLabel u)   -> pprUniqueAlways u <> text "_str"
   (SRTLabel u)         -> tempLabelPrefixOrUnderscore platform <> pprUniqueAlways u <> pp_cSEP <> text "srt"
   (LargeBitmapLabel u) -> tempLabelPrefixOrUnderscore platform
                           <> char 'b' <> pprUniqueAlways u <> pp_cSEP <> text "btm"
                           -- Some bitmaps for tuple constructors have a numeric tag (e.g. '7')
                           -- until that gets resolved we'll just force them to start
                           -- with a letter so the label will be legal assembly code.

   (CmmLabel _ _ str CmmCode)     -> ftext str
   (CmmLabel _ _ str CmmData)     -> ftext str
   (CmmLabel _ _ str CmmPrimCall) -> ftext str

   (LocalBlockLabel u) -> tempLabelPrefixOrUnderscore platform <> text "blk_" <> pprUniqueAlways u

   (RtsLabel (RtsApFast str)) -> ftext str <> text "_fast"

   (RtsLabel (RtsSelectorInfoTable upd_reqd offset)) ->
    hcat [text "stg_sel_", text (show offset),
          ptext (if upd_reqd
                 then (sLit "_upd_info")
                 else (sLit "_noupd_info"))
        ]

   (RtsLabel (RtsSelectorEntry upd_reqd offset)) ->
    hcat [text "stg_sel_", text (show offset),
                ptext (if upd_reqd
                        then (sLit "_upd_entry")
                        else (sLit "_noupd_entry"))
        ]

   (RtsLabel (RtsApInfoTable upd_reqd arity)) ->
    hcat [text "stg_ap_", text (show arity),
                ptext (if upd_reqd
                        then (sLit "_upd_info")
                        else (sLit "_noupd_info"))
        ]

   (RtsLabel (RtsApEntry upd_reqd arity)) ->
    hcat [text "stg_ap_", text (show arity),
                ptext (if upd_reqd
                        then (sLit "_upd_entry")
                        else (sLit "_noupd_entry"))
        ]

   (CmmLabel _ _ fs CmmInfo)    -> ftext fs <> text "_info"
   (CmmLabel _ _ fs CmmEntry)   -> ftext fs <> text "_entry"
   (CmmLabel _ _ fs CmmRetInfo) -> ftext fs <> text "_info"
   (CmmLabel _ _ fs CmmRet)     -> ftext fs <> text "_ret"
   (CmmLabel _ _ fs CmmClosure) -> ftext fs <> text "_closure"

   (RtsLabel (RtsPrimOp primop)) -> text "stg_" <> ppr primop
   (RtsLabel (RtsSlowFastTickyCtr pat)) ->
      text "SLOW_CALL_fast_" <> text pat <> ptext (sLit "_ctr")

   (ForeignLabel str _ _ _) -> ftext str

   (IdLabel name _cafs flavor) -> internalNamePrefix platform name <> ppr name <> ppIdFlavor flavor

   (CC_Label cc)       -> ppr cc
   (CCS_Label ccs)     -> ppr ccs
   (HpcTicksLabel mod) -> text "_hpc_tickboxes_"  <> ppr mod <> ptext (sLit "_hpc")

   (AsmTempLabel {})        -> panic "pprCLbl AsmTempLabel"
   (AsmTempDerivedLabel {}) -> panic "pprCLbl AsmTempDerivedLabel"
   (DynamicLinkerLabel {})  -> panic "pprCLbl DynamicLinkerLabel"
   (PicBaseLabel {})        -> panic "pprCLbl PicBaseLabel"
   (DeadStripPreventer {})  -> panic "pprCLbl DeadStripPreventer"

ppIdFlavor :: IdLabelInfo -> SDoc
ppIdFlavor x = pp_cSEP <> text
               (case x of
                       Closure          -> "closure"
                       InfoTable        -> "info"
                       LocalInfoTable   -> "info"
                       Entry            -> "entry"
                       LocalEntry       -> "entry"
                       Slow             -> "slow"
                       RednCounts       -> "ct"
                       ConEntry         -> "con_entry"
                       ConInfoTable     -> "con_info"
                       ClosureTable     -> "closure_tbl"
                       Bytes            -> "bytes"
                       BlockInfoTable   -> "info"
                      )


pp_cSEP :: SDoc
pp_cSEP = char '_'


instance Outputable ForeignLabelSource where
 ppr fs
  = case fs of
        ForeignLabelInPackage pkgId     -> parens $ text "package: " <> ppr pkgId
        ForeignLabelInThisPackage       -> parens $ text "this package"
        ForeignLabelInExternalPackage   -> parens $ text "external package"

internalNamePrefix :: Platform -> Name -> SDoc
internalNamePrefix platform name = getPprStyle $ \ sty ->
  if asmStyle sty && isRandomGenerated then
      ptext (asmTempLabelPrefix platform)
  else
    empty
  where
    isRandomGenerated = not $ isExternalName name

tempLabelPrefixOrUnderscore :: Platform -> SDoc
tempLabelPrefixOrUnderscore platform =
  getPprStyle $ \ sty ->
   if asmStyle sty then
      ptext (asmTempLabelPrefix platform)
   else
      char '_'

-- -----------------------------------------------------------------------------
-- Machine-dependent knowledge about labels.

asmTempLabelPrefix :: Platform -> PtrString  -- for formatting labels
asmTempLabelPrefix platform = case platformOS platform of
    OSDarwin -> sLit "L"
    OSAIX    -> sLit "__L" -- follow IBM XL C's convention
    _        -> sLit ".L"

pprDynamicLinkerAsmLabel :: Platform -> DynamicLinkerLabelInfo -> CLabel -> SDoc
pprDynamicLinkerAsmLabel platform dllInfo lbl =
    case platformOS platform of
      OSDarwin
        | platformArch platform == ArchX86_64 ->
          case dllInfo of
            CodeStub        -> char 'L' <> ppr lbl <> text "$stub"
            SymbolPtr       -> char 'L' <> ppr lbl <> text "$non_lazy_ptr"
            GotSymbolPtr    -> ppr lbl <> text "@GOTPCREL"
            GotSymbolOffset -> ppr lbl
        | otherwise ->
          case dllInfo of
            CodeStub  -> char 'L' <> ppr lbl <> text "$stub"
            SymbolPtr -> char 'L' <> ppr lbl <> text "$non_lazy_ptr"
            _         -> panic "pprDynamicLinkerAsmLabel"

      OSAIX ->
          case dllInfo of
            SymbolPtr -> text "LC.." <> ppr lbl -- GCC's naming convention
            _         -> panic "pprDynamicLinkerAsmLabel"

      _ | osElfTarget (platformOS platform) -> elfLabel

      OSMinGW32 ->
          case dllInfo of
            SymbolPtr -> text "__imp_" <> ppr lbl
            _         -> panic "pprDynamicLinkerAsmLabel"

      _ -> panic "pprDynamicLinkerAsmLabel"
  where
    elfLabel
      | platformArch platform == ArchPPC
      = case dllInfo of
          CodeStub  -> -- See Note [.LCTOC1 in PPC PIC code]
                       ppr lbl <> text "+32768@plt"
          SymbolPtr -> text ".LC_" <> ppr lbl
          _         -> panic "pprDynamicLinkerAsmLabel"

      | platformArch platform == ArchX86_64
      = case dllInfo of
          CodeStub        -> ppr lbl <> text "@plt"
          GotSymbolPtr    -> ppr lbl <> text "@gotpcrel"
          GotSymbolOffset -> ppr lbl
          SymbolPtr       -> text ".LC_" <> ppr lbl

      | platformArch platform == ArchPPC_64 ELF_V1
        || platformArch platform == ArchPPC_64 ELF_V2
      = case dllInfo of
          GotSymbolPtr    -> text ".LC_"  <> ppr lbl
                                  <> text "@toc"
          GotSymbolOffset -> ppr lbl
          SymbolPtr       -> text ".LC_" <> ppr lbl
          _               -> panic "pprDynamicLinkerAsmLabel"

      | otherwise
      = case dllInfo of
          CodeStub        -> ppr lbl <> text "@plt"
          SymbolPtr       -> text ".LC_" <> ppr lbl
          GotSymbolPtr    -> ppr lbl <> text "@got"
          GotSymbolOffset -> ppr lbl <> text "@gotoff"

-- Figure out whether `symbol` may serve as an alias
-- to `target` within one compilation unit.
--
-- This is true if any of these holds:
-- * `target` is a module-internal haskell name.
-- * `target` is an exported name, but comes from the same
--   module as `symbol`
--
-- These are sufficient conditions for establishing e.g. a
-- GNU assembly alias ('.equiv' directive). Sadly, there is
-- no such thing as an alias to an imported symbol (conf.
-- http://blog.omega-prime.co.uk/2011/07/06/the-sad-state-of-symbol-aliases/)
-- See note [emit-time elimination of static indirections].
--
-- Precondition is that both labels represent the
-- same semantic value.

mayRedirectTo :: CLabel -> CLabel -> Bool
mayRedirectTo symbol target
 | Just nam <- haskellName
 , staticClosureLabel
 , isExternalName nam
 , Just mod <- nameModule_maybe nam
 , Just anam <- hasHaskellName symbol
 , Just amod <- nameModule_maybe anam
 = amod == mod

 | Just nam <- haskellName
 , staticClosureLabel
 , isInternalName nam
 = True

 | otherwise = False
   where staticClosureLabel = isStaticClosureLabel target
         haskellName = hasHaskellName target


{-
Note [emit-time elimination of static indirections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in #15155, certain static values are representationally
equivalent, e.g. 'cast'ed values (when created by 'newtype' wrappers).

             newtype A = A Int
             {-# NOINLINE a #-}
             a = A 42

a1_rYB :: Int
[GblId, Caf=NoCafRefs, Unf=OtherCon []]
a1_rYB = GHC.Types.I# 42#

a [InlPrag=NOINLINE] :: A
[GblId, Unf=OtherCon []]
a = a1_rYB `cast` (Sym (T15155.N:A[0]) :: Int ~R# A)

Formerly we created static indirections for these (IND_STATIC), which
consist of a statically allocated forwarding closure that contains
the (possibly tagged) indirectee. (See CMM/assembly below.)
This approach is suboptimal for two reasons:
  (a) they occupy extra space,
  (b) they need to be entered in order to obtain the indirectee,
      thus they cannot be tagged.

Fortunately there is a common case where static indirections can be
eliminated while emitting assembly (native or LLVM), viz. when the
indirectee is in the same module (object file) as the symbol that
points to it. In this case an assembly-level identification can
be created ('.equiv' directive), and as such the same object will
be assigned two names in the symbol table. Any of the identified
symbols can be referenced by a tagged pointer.

Currently the 'mayRedirectTo' predicate will
give a clue whether a label can be equated with another, already
emitted, label (which can in turn be an alias). The general mechanics
is that we identify data (IND_STATIC closures) that are amenable
to aliasing while pretty-printing of assembly output, and emit the
'.equiv' directive instead of static data in such a case.

Here is a sketch how the output is massaged:

                     Consider
newtype A = A Int
{-# NOINLINE a #-}
a = A 42                                -- I# 42# is the indirectee
                                        -- 'a' is exported

                 results in STG

a1_rXq :: GHC.Types.Int
[GblId, Caf=NoCafRefs, Unf=OtherCon []] =
    CCS_DONT_CARE GHC.Types.I#! [42#];

T15155.a [InlPrag=NOINLINE] :: T15155.A
[GblId, Unf=OtherCon []] =
    CAF_ccs  \ u  []  a1_rXq;

                 and CMM

[section ""data" . a1_rXq_closure" {
     a1_rXq_closure:
         const GHC.Types.I#_con_info;
         const 42;
 }]

[section ""data" . T15155.a_closure" {
     T15155.a_closure:
         const stg_IND_STATIC_info;
         const a1_rXq_closure+1;
         const 0;
         const 0;
 }]

The emitted assembly is

#### INDIRECTEE
a1_rXq_closure:                         -- module local haskell value
        .quad   GHC.Types.I#_con_info   -- an Int
        .quad   42

#### BEFORE
.globl T15155.a_closure                 -- exported newtype wrapped value
T15155.a_closure:
        .quad   stg_IND_STATIC_info     -- the closure info
        .quad   a1_rXq_closure+1        -- indirectee ('+1' being the tag)
        .quad   0
        .quad   0

#### AFTER
.globl T15155.a_closure                 -- exported newtype wrapped value
.equiv a1_rXq_closure,T15155.a_closure  -- both are shared

The transformation is performed because
     T15155.a_closure `mayRedirectTo` a1_rXq_closure+1
returns True.
-}
