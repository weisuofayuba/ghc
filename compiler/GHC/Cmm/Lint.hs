-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2011
--
-- CmmLint: checking the correctness of Cmm statements and expressions
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module GHC.Cmm.Lint (
    cmmLint, cmmLintGraph
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Regs (callerSaves)
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Liveness
import GHC.Cmm.Switch (switchTargetsToList)
import GHC.Cmm.Ppr () -- For Outputable instances
import GHC.Utils.Outputable
import GHC.Driver.Session

import Control.Monad (ap, unless)

-- Things to check:
--     - invariant on CmmBlock in GHC.Cmm.Expr (see comment there)
--     - check for branches to blocks that don't exist
--     - check types

-- -----------------------------------------------------------------------------
-- Exported entry points:

cmmLint :: (Outputable d, Outputable h)
        => DynFlags -> GenCmmGroup d h CmmGraph -> Maybe SDoc
cmmLint dflags tops = runCmmLint dflags (mapM_ (lintCmmDecl dflags)) tops

cmmLintGraph :: DynFlags -> CmmGraph -> Maybe SDoc
cmmLintGraph dflags g = runCmmLint dflags (lintCmmGraph dflags) g

runCmmLint :: Outputable a => DynFlags -> (a -> CmmLint b) -> a -> Maybe SDoc
runCmmLint dflags l p =
   case unCL (l p) dflags of
     Left err -> Just (vcat [text "Cmm lint error:",
                             nest 2 err,
                             text "Program was:",
                             nest 2 (ppr p)])
     Right _  -> Nothing

lintCmmDecl :: DynFlags -> GenCmmDecl h i CmmGraph -> CmmLint ()
lintCmmDecl dflags (CmmProc _ lbl _ g)
  = addLintInfo (text "in proc " <> ppr lbl) $ lintCmmGraph dflags g
lintCmmDecl _ (CmmData {})
  = return ()


lintCmmGraph :: DynFlags -> CmmGraph -> CmmLint ()
lintCmmGraph dflags g =
    cmmLocalLiveness dflags g `seq` mapM_ (lintCmmBlock labels) blocks
    -- cmmLiveness throws an error if there are registers
    -- live on entry to the graph (i.e. undefined
    -- variables)
  where
       blocks = toBlockList g
       labels = setFromList (map entryLabel blocks)


lintCmmBlock :: LabelSet -> CmmBlock -> CmmLint ()
lintCmmBlock labels block
  = addLintInfo (text "in basic block " <> ppr (entryLabel block)) $ do
        let (_, middle, last) = blockSplit block
        mapM_ lintCmmMiddle (blockToList middle)
        lintCmmLast labels last

-- -----------------------------------------------------------------------------
-- lintCmmExpr

-- Checks whether a CmmExpr is "type-correct", and check for obvious-looking
-- byte/word mismatches.

lintCmmExpr :: CmmExpr -> CmmLint CmmType
lintCmmExpr (CmmLoad expr rep) = do
  _ <- lintCmmExpr expr
  -- Disabled, if we have the inlining phase before the lint phase,
  -- we can have funny offsets due to pointer tagging. -- EZY
  -- when (widthInBytes (typeWidth rep) >= platformWordSizeInBytes platform) $
  --   cmmCheckWordAddress expr
  return rep
lintCmmExpr expr@(CmmMachOp op args) = do
  platform <- getPlatform
  tys <- mapM lintCmmExpr args
  if map (typeWidth . cmmExprType platform) args == machOpArgReps platform op
        then cmmCheckMachOp op args tys
        else cmmLintMachOpErr expr (map (cmmExprType platform) args) (machOpArgReps platform op)
lintCmmExpr (CmmRegOff reg offset)
  = do platform <- getPlatform
       let rep = typeWidth (cmmRegType platform reg)
       lintCmmExpr (CmmMachOp (MO_Add rep)
                [CmmReg reg, CmmLit (CmmInt (fromIntegral offset) rep)])
lintCmmExpr expr =
  do platform <- getPlatform
     return (cmmExprType platform expr)

-- Check for some common byte/word mismatches (eg. Sp + 1)
cmmCheckMachOp   :: MachOp -> [CmmExpr] -> [CmmType] -> CmmLint CmmType
cmmCheckMachOp op [lit@(CmmLit (CmmInt { })), reg@(CmmReg _)] tys
  = cmmCheckMachOp op [reg, lit] tys
cmmCheckMachOp op _ tys
  = do platform <- getPlatform
       return (machOpResultType platform op tys)

{-
isOffsetOp :: MachOp -> Bool
isOffsetOp (MO_Add _) = True
isOffsetOp (MO_Sub _) = True
isOffsetOp _ = False

-- This expression should be an address from which a word can be loaded:
-- check for funny-looking sub-word offsets.
_cmmCheckWordAddress :: CmmExpr -> CmmLint ()
_cmmCheckWordAddress e@(CmmMachOp op [arg, CmmLit (CmmInt i _)])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (platformWordSizeInBytes platform) /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress e@(CmmMachOp op [CmmLit (CmmInt i _), arg])
  | isOffsetOp op && notNodeReg arg && i `rem` fromIntegral (platformWordSizeInBytes platform) /= 0
  = cmmLintDubiousWordOffset e
_cmmCheckWordAddress _
  = return ()

-- No warnings for unaligned arithmetic with the node register,
-- which is used to extract fields from tagged constructor closures.
notNodeReg :: CmmExpr -> Bool
notNodeReg (CmmReg reg) | reg == nodeReg = False
notNodeReg _                             = True
-}

lintCmmMiddle :: CmmNode O O -> CmmLint ()
lintCmmMiddle node = case node of
  CmmComment _ -> return ()
  CmmTick _    -> return ()
  CmmUnwind{}  -> return ()

  CmmAssign reg expr -> do
            platform <- getPlatform
            erep <- lintCmmExpr expr
            let reg_ty = cmmRegType platform reg
            if (erep `cmmEqType_ignoring_ptrhood` reg_ty)
                then return ()
                else cmmLintAssignErr (CmmAssign reg expr) erep reg_ty

  CmmStore l r -> do
            _ <- lintCmmExpr l
            _ <- lintCmmExpr r
            return ()

  CmmUnsafeForeignCall target _formals actuals -> do
            lintTarget target
            let lintArg expr = do
                  -- Arguments can't mention caller-saved
                  -- registers. See Note [Register parameter passing].
                  mayNotMentionCallerSavedRegs (text "foreign call argument") expr
                  lintCmmExpr expr

            mapM_ lintArg actuals


lintCmmLast :: LabelSet -> CmmNode O C -> CmmLint ()
lintCmmLast labels node = case node of
  CmmBranch id -> checkTarget id

  CmmCondBranch e t f _ -> do
            platform <- getPlatform
            mapM_ checkTarget [t,f]
            _ <- lintCmmExpr e
            checkCond platform e

  CmmSwitch e ids -> do
            platform <- getPlatform
            mapM_ checkTarget $ switchTargetsToList ids
            erep <- lintCmmExpr e
            if (erep `cmmEqType_ignoring_ptrhood` bWord platform)
              then return ()
              else cmmLintErr (text "switch scrutinee is not a word: " <>
                               ppr e <> text " :: " <> ppr erep)

  CmmCall { cml_target = target, cml_cont = cont } -> do
          _ <- lintCmmExpr target
          maybe (return ()) checkTarget cont

  CmmForeignCall tgt _ args succ _ _ _ -> do
          lintTarget tgt
          let lintArg expr = do
                -- Arguments can't mention caller-saved
                -- registers. See Note [Register
                -- parameter passing].
                -- N.B. This won't catch local registers
                -- which the NCG's register allocator later
                -- places in caller-saved registers.
                mayNotMentionCallerSavedRegs (text "foreign call argument") expr
                lintCmmExpr expr
          mapM_ lintArg args
          checkTarget succ
 where
  checkTarget id
     | setMember id labels = return ()
     | otherwise = cmmLintErr (text "Branch to nonexistent id" <+> ppr id)

lintTarget :: ForeignTarget -> CmmLint ()
lintTarget (ForeignTarget e _) = do
    mayNotMentionCallerSavedRegs (text "foreign target") e
    _ <- lintCmmExpr e
    return ()
lintTarget (PrimTarget {})     = return ()

-- | As noted in Note [Register parameter passing], the arguments and
-- 'ForeignTarget' of a foreign call mustn't mention
-- caller-saved registers.
mayNotMentionCallerSavedRegs :: (UserOfRegs GlobalReg a, Outputable a)
                             => SDoc -> a -> CmmLint ()
mayNotMentionCallerSavedRegs what thing = do
    dflags <- getDynFlags
    let badRegs = filter (callerSaves (targetPlatform dflags))
                  $ foldRegsUsed dflags (flip (:)) [] thing
    unless (null badRegs)
      $ cmmLintErr (what <+> text "mentions caller-saved registers: " <> ppr badRegs $$ ppr thing)

checkCond :: Platform -> CmmExpr -> CmmLint ()
checkCond _ (CmmMachOp mop _) | isComparisonMachOp mop = return ()
checkCond platform (CmmLit (CmmInt x t)) | x == 0 || x == 1, t == wordWidth platform = return () -- constant values
checkCond _ expr
    = cmmLintErr (hang (text "expression is not a conditional:") 2
                         (ppr expr))

-- -----------------------------------------------------------------------------
-- CmmLint monad

-- just a basic error monad:

newtype CmmLint a = CmmLint { unCL :: DynFlags -> Either SDoc a }
    deriving (Functor)

instance Applicative CmmLint where
      pure a = CmmLint (\_ -> Right a)
      (<*>) = ap

instance Monad CmmLint where
  CmmLint m >>= k = CmmLint $ \dflags ->
                                case m dflags of
                                Left e -> Left e
                                Right a -> unCL (k a) dflags

instance HasDynFlags CmmLint where
    getDynFlags = CmmLint (\dflags -> Right dflags)

getPlatform :: CmmLint Platform
getPlatform = targetPlatform <$> getDynFlags

cmmLintErr :: SDoc -> CmmLint a
cmmLintErr msg = CmmLint (\_ -> Left msg)

addLintInfo :: SDoc -> CmmLint a -> CmmLint a
addLintInfo info thing = CmmLint $ \dflags ->
   case unCL thing dflags of
        Left err -> Left (hang info 2 err)
        Right a  -> Right a

cmmLintMachOpErr :: CmmExpr -> [CmmType] -> [Width] -> CmmLint a
cmmLintMachOpErr expr argsRep opExpectsRep
     = cmmLintErr (text "in MachOp application: " $$
                   nest 2 (ppr  expr) $$
                      (text "op is expecting: " <+> ppr opExpectsRep) $$
                      (text "arguments provide: " <+> ppr argsRep))

cmmLintAssignErr :: CmmNode e x -> CmmType -> CmmType -> CmmLint a
cmmLintAssignErr stmt e_ty r_ty
  = cmmLintErr (text "in assignment: " $$
                nest 2 (vcat [ppr stmt,
                              text "Reg ty:" <+> ppr r_ty,
                              text "Rhs ty:" <+> ppr e_ty]))


{-
cmmLintDubiousWordOffset :: CmmExpr -> CmmLint a
cmmLintDubiousWordOffset expr
   = cmmLintErr (text "offset is not a multiple of words: " $$
                 nest 2 (ppr expr))
-}

