{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Annotate a CmmGraph with ThreadSanitizer instrumentation calls.
module GHC.Cmm.ThreadSanitizer (annotateTSAN) where

import GHC.Prelude

import GHC.Platform
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Types.ForeignCall

annotateTSAN :: Platform -> CmmDecl -> CmmDecl
annotateTSAN platform (CmmProc hdr lbl regs g) =
    CmmProc hdr lbl regs (annotateGraph platform g)
annotateTSAN _platform decl = decl

annotateGraph :: Platform -> CmmGraph -> CmmGraph
annotateGraph _platform graph =
    modifyGraph (mapGraphBlocks annotateBlock) graph

mapBlockList :: (forall e' x'. n e' x' -> Block n e' x')
             -> Block n e x -> Block n e x
mapBlockList f (BlockCO n rest  ) = f n `blockAppend` mapBlockList f rest
mapBlockList f (BlockCC n rest m) = f n `blockAppend` mapBlockList f rest `blockAppend` f m
mapBlockList f (BlockOC   rest m) = mapBlockList f rest `blockAppend` f m
mapBlockList _ BNil = BNil
mapBlockList f (BMiddle blk) = f blk
mapBlockList f (BCat a b) = mapBlockList f a `blockAppend` mapBlockList f b
mapBlockList f (BSnoc a n) = mapBlockList f a `blockAppend` f n
mapBlockList f (BCons n a) = f n `blockAppend` mapBlockList f a

annotateBlock :: Block CmmNode e x -> Block CmmNode e x
annotateBlock = mapBlockList annotateNode

annotateNode :: CmmNode e x -> Block CmmNode e x
annotateNode node@(CmmEntry{}) = BlockCO node BNil
annotateNode node@(CmmComment{}) = BMiddle node
annotateNode node@(CmmTick{}) = BMiddle node
annotateNode node@(CmmUnwind{}) = BMiddle node
annotateNode node@(CmmAssign{}) = annotateNodeOO node
annotateNode node@(CmmStore dest _) =
    blockFromList (map tsanLoad (collectLoadsNode node) ++ [tsanStore dest]) `blockSnoc` node
annotateNode node@(CmmUnsafeForeignCall (PrimTarget op) _formals args) =
    blockFromList (annotatePrim op args ++ foldMap annotateExpr args) `blockSnoc` node
annotateNode node@(CmmUnsafeForeignCall{}) = annotateNodeOO node
annotateNode node@(CmmBranch{}) = annotateNodeOC node
annotateNode node@(CmmCondBranch{}) = annotateNodeOC node
annotateNode node@(CmmSwitch{}) = annotateNodeOC node
annotateNode node@(CmmCall{}) = annotateNodeOC node
annotateNode node@(CmmForeignCall{}) = annotateNodeOC node

annotateNodeOO :: CmmNode O O -> Block CmmNode O O
annotateNodeOO node =
    blockFromList (map tsanLoad (collectLoadsNode node)) `blockSnoc` node

annotateNodeOC :: CmmNode O C -> Block CmmNode O C
annotateNodeOC node =
    blockFromList (map tsanLoad (collectLoadsNode node)) `blockJoinTail` node

annotateExpr :: CmmExpr -> [CmmNode O O]
annotateExpr expr =
    map tsanLoad (collectExprLoads expr)

annotatePrim :: CallishMachOp -> [CmmActual] -> [CmmNode O O]
annotatePrim MO_ReadBarrier _args = [] -- TODO
annotatePrim MO_WriteBarrier _args = [] -- TODO
annotatePrim (MO_AtomicRMW _w _op) _args = [] -- TODO
annotatePrim (MO_AtomicRead _w) _args = [] -- TODO
annotatePrim (MO_AtomicWrite _w) _args = [] -- TODO
annotatePrim (MO_Cmpxchg _w) _args = [] -- TODO
annotatePrim (MO_Xchg _w) _args = [] -- TODO
annotatePrim _ _ = []

collectLoadsNode :: CmmNode e x -> [CmmExpr]
collectLoadsNode node =
    foldExp (\exp rest -> collectExprLoads exp ++ rest) node []

-- | Collect all of the memory locations loaded from by a 'CmmExpr'.
collectExprLoads :: CmmExpr -> [CmmExpr]
collectExprLoads (CmmLit _) = []
collectExprLoads (CmmLoad e _ty) = [e]
collectExprLoads (CmmReg _) = []
collectExprLoads (CmmMachOp _op args) = foldMap collectExprLoads args -- TODO
collectExprLoads (CmmStackSlot _ _) = []
collectExprLoads (CmmRegOff _ _) = []

tsanStore :: CmmExpr -> CmmNode O O
tsanStore dest =
    CmmUnsafeForeignCall ftarget [] [dest]
  where
    ftarget = ForeignTarget (CmmLit (CmmLabel lbl)) conv
    conv = ForeignConvention CCallConv [AddrHint] [] CmmMayReturn
    lbl = mkForeignLabel (fsLit "__tsan_write8") Nothing ForeignLabelInExternalPackage IsFunction

tsanLoad :: CmmExpr -> CmmNode O O
tsanLoad dest =
    CmmUnsafeForeignCall ftarget [] [dest]
  where
    ftarget = ForeignTarget (CmmLit (CmmLabel lbl)) conv
    conv = ForeignConvention CCallConv [AddrHint] [] CmmMayReturn
    lbl = mkForeignLabel (fsLit "__tsan_read8") Nothing ForeignLabelInExternalPackage IsFunction
