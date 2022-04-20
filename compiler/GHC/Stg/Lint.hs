{- |
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

A lint pass to check basic STG invariants:

- Variables should be defined before used.

- Let bindings should not have unboxed types (unboxed bindings should only
  appear in case), except when they're join points (see Note [Core let-can-float
  invariant] and #14117).

- If linting after unarisation, invariants listed in Note [Post-unarisation
  invariants].

Because we don't have types and coercions in STG we can't really check types
here.

Some history:

StgLint used to check types, but it never worked and so it was disabled in 2000
with this note:

    WARNING:
    ~~~~~~~~

    This module has suffered bit-rot; it is likely to yield lint errors
    for Stg code that is currently perfectly acceptable for code
    generation.  Solution: don't use it!  (KSW 2000-05).

Since then there were some attempts at enabling it again, as summarised in #14787.
It's finally decided that we remove all type checking and only look for
basic properties listed above.

Note [Linting StgApp]
~~~~~~~~~~~~~~~~~~~~~
When looking at an application of the form `f a1 ... an` it's possible
for `f` to be of a runtime polymorphic type. See https://gitlab.haskell.org/ghc/ghc/-/issues/21399
for an example.
So for now we just avoid linting apps of such types.

For monomorphic types we check that for
    f :: ty1 -> .. -> tyn

    foo = .... f a1 .. an

That for each ai and tyi it holds that
    runtimeRepCompatible (typePrimRep tyi) (typePrimRep (idType ai))
This will allow lint to catch issues like #21396.

Note [Typing the STG language]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Core, programs must be /well-typed/.  So if f :: ty1 -> ty2,
then in the application (f e), we must have  e :: ty1

STG is still a statically typed language, but the type system
is much coarser. In particular, STG programs must be /well-kinded/.
More precisely, if f :: ty1 -> ty2, then in the application (f e)
where e :: ty1', we must have kind(ty1) = kind(ty1').

So the STG type system does not distinguish beteen Int and Bool,
but it /does/ distinguish beteen Int and Int#, because they have
different kinds.  Actually, since all terms have kind (TYPE rep),
we might say that the STG language is well-runtime-rep'd.

This coarser type system makes fewer distinctions, and that allows
many nonsensical programs (such as ('x' && "foo")) -- but all type
systems accept buggy programs!  But the coarseness also permits
some optimisations that are ill-typed in Core.  For example, see
the module STG.CSE, which is all about doing CSE in STG that would
be ill-typed in Core.  But it must still be well-kinded!

-}

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  DeriveFunctor #-}

module GHC.Stg.Lint ( lintStgTopBindings ) where

import GHC.Prelude

import GHC.Stg.Syntax
import GHC.Stg.Utils

import GHC.Core.Lint        ( interactiveInScope )
import GHC.Core.DataCon
import GHC.Core             ( AltCon(..) )
import GHC.Core.Type

import GHC.Types.Basic      ( TopLevelFlag(..), isTopLevel, isMarkedCbv )
import GHC.Types.CostCentre ( isCurrentCCS )
import GHC.Types.Error      ( DiagnosticReason(WarningWithoutFlag) )
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Name       ( getSrcLoc, nameIsLocalOrFrom )
import GHC.Types.RepType
import GHC.Types.SrcLoc

import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Error      ( mkLocMessage, DiagOpts )
import qualified GHC.Utils.Error as Err

import GHC.Unit.Module            ( Module )
import GHC.Runtime.Context        ( InteractiveContext )

import GHC.Data.Bag         ( Bag, emptyBag, isEmptyBag, snocBag, bagToList )

import Control.Applicative ((<|>))
import Control.Monad
import Data.Maybe
import GHC.Utils.Misc
import GHC.Core.Multiplicity (scaledThing)
import GHC.Settings (Platform)
import GHC.Core.TyCon (primRepCompatible)
import GHC.Data.Maybe
import GHC.Utils.Panic.Plain (panic)

lintStgTopBindings :: forall a . (OutputablePass a, BinderP a ~ Id)
                   => Platform
                   -> Logger
                   -> DiagOpts
                   -> StgPprOpts
                   -> InteractiveContext
                   -> Module -- ^ module being compiled
                   -> Bool   -- ^ have we run Unarise yet?
                   -> String -- ^ who produced the STG?
                   -> [GenStgTopBinding a]
                   -> IO ()

lintStgTopBindings platform logger diag_opts opts ictxt this_mod unarised whodunnit binds
  = {-# SCC "StgLint" #-}
    case initL platform diag_opts this_mod unarised opts top_level_binds (lint_binds binds) of
      Nothing  ->
        return ()
      Just msg -> do
        logMsg logger Err.MCDump noSrcSpan
          $ withPprStyle defaultDumpStyle
          (vcat [ text "*** Stg Lint ErrMsgs: in" <+>
                        text whodunnit <+> text "***",
                  msg,
                  text "*** Offending Program ***",
                  pprGenStgTopBindings opts binds,
                  text "*** End of Offense ***"])
        Err.ghcExit logger 1
  where
    -- Bring all top-level binds into scope because CoreToStg does not generate
    -- bindings in dependency order (so we may see a use before its definition).
    top_level_binds = extendVarSetList (mkVarSet (bindersOfTopBinds binds))
                                       (interactiveInScope ictxt)

    lint_binds :: [GenStgTopBinding a] -> LintM ()

    lint_binds [] = return ()
    lint_binds (bind:binds) = do
        binders <- lint_bind bind
        addInScopeVars binders $
            lint_binds binds

    lint_bind (StgTopLifted bind) = lintStgBinds TopLevel bind
    lint_bind (StgTopStringLit v _) = return [v]

lintStgArg :: StgArg -> LintM ()
lintStgArg (StgLitArg _) = return ()
lintStgArg (StgVarArg v) = lintStgVar v

lintStgVar :: Id -> LintM ()
lintStgVar id = checkInScope id

lintStgBinds
    :: (OutputablePass a, BinderP a ~ Id)
    => TopLevelFlag -> GenStgBinding a -> LintM [Id] -- Returns the binders
lintStgBinds top_lvl (StgNonRec binder rhs) = do
    lint_binds_help top_lvl (binder,rhs)
    return [binder]

lintStgBinds top_lvl (StgRec pairs)
  = addInScopeVars binders $ do
        mapM_ (lint_binds_help top_lvl) pairs
        return binders
  where
    binders = [b | (b,_) <- pairs]

lint_binds_help
    :: (OutputablePass a, BinderP a ~ Id)
    => TopLevelFlag
    -> (Id, GenStgRhs a)
    -> LintM ()
lint_binds_help top_lvl (binder, rhs)
  = addLoc (RhsOf binder) $ do
        when (isTopLevel top_lvl) (checkNoCurrentCCS rhs)
        lintStgRhs rhs
        opts <- getStgPprOpts
        -- Check binder doesn't have unlifted type or it's a join point
        checkL ( isJoinId binder
              || not (isUnliftedType (idType binder))
              || isDataConWorkId binder || isDataConWrapId binder) -- until #17521 is fixed
          (mkUnliftedTyMsg opts binder rhs)

-- | Top-level bindings can't inherit the cost centre stack from their
-- (static) allocation site.
checkNoCurrentCCS
    :: (OutputablePass a, BinderP a ~ Id)
    => GenStgRhs a
    -> LintM ()
checkNoCurrentCCS rhs = do
   opts <- getStgPprOpts
   let rhs' = pprStgRhs opts rhs
   case rhs of
      StgRhsClosure _ ccs _ _ _
         | isCurrentCCS ccs
         -> addErrL (text "Top-level StgRhsClosure with CurrentCCS" $$ rhs')
      StgRhsCon ccs _ _ _ _
         | isCurrentCCS ccs
         -> addErrL (text "Top-level StgRhsCon with CurrentCCS" $$ rhs')
      _ -> return ()

lintStgRhs :: (OutputablePass a, BinderP a ~ Id) => GenStgRhs a -> LintM ()

lintStgRhs (StgRhsClosure _ _ _ [] expr)
  = lintStgExpr expr

lintStgRhs (StgRhsClosure _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) $
      addInScopeVars binders $
        lintStgExpr expr

lintStgRhs rhs@(StgRhsCon _ con _ _ args) = do
    opts <- getStgPprOpts
    when (isUnboxedTupleDataCon con || isUnboxedSumDataCon con) $ do
      addErrL (text "StgRhsCon is an unboxed tuple or sum application" $$
               pprStgRhs opts rhs)

    lintConApp con args (pprStgRhs opts rhs)

    mapM_ lintStgArg args
    mapM_ checkPostUnariseConArg args

lintStgExpr :: (OutputablePass a, BinderP a ~ Id) => GenStgExpr a -> LintM ()

lintStgExpr (StgLit _) = return ()

lintStgExpr e@(StgApp fun args) = do
  lintStgVar fun
  mapM_ lintStgArg args
  lintAppCbvMarks e
  lintStgAppReps fun args



lintStgExpr app@(StgConApp con _n args _arg_tys) = do
    -- unboxed sums should vanish during unarise
    lf <- getLintFlags
    let !unarised = lf_unarised lf
    when (unarised && isUnboxedSumDataCon con) $ do
      opts <- getStgPprOpts
      addErrL (text "Unboxed sum after unarise:" $$
               pprStgExpr opts app)

    opts <- getStgPprOpts
    lintConApp con args (pprStgExpr opts app)

    mapM_ lintStgArg args
    mapM_ checkPostUnariseConArg args

lintStgExpr (StgOpApp _ args _) =
    mapM_ lintStgArg args

lintStgExpr (StgLet _ binds body) = do
    binders <- lintStgBinds NotTopLevel binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgLetNoEscape _ binds body) = do
    binders <- lintStgBinds NotTopLevel binds
    addLoc (BodyOfLetRec binders) $
      addInScopeVars binders $
        lintStgExpr body

lintStgExpr (StgTick _ expr) = lintStgExpr expr

lintStgExpr (StgCase scrut bndr alts_type alts) = do
    lintStgExpr scrut

    lf <- getLintFlags
    let in_scope = stgCaseBndrInScope alts_type (lf_unarised lf)

    addInScopeVars [bndr | in_scope] (mapM_ lintAlt alts)

lintAlt
    :: (OutputablePass a, BinderP a ~ Id)
    => GenStgAlt a -> LintM ()

lintAlt GenStgAlt{ alt_con   = DEFAULT
                 , alt_bndrs = _
                 , alt_rhs   = rhs} = lintStgExpr rhs

lintAlt GenStgAlt{ alt_con   = LitAlt _
                 , alt_bndrs = _
                 , alt_rhs   = rhs} = lintStgExpr rhs

lintAlt GenStgAlt{ alt_con   = DataAlt _
                 , alt_bndrs = bndrs
                 , alt_rhs   = rhs} =
  do
    mapM_ checkPostUnariseBndr bndrs
    addInScopeVars bndrs (lintStgExpr rhs)

-- Post unarise check we apply constructors to the right number of args.
-- This can be violated by invalid use of unsafeCoerce as showcased by test
-- T9208
lintConApp :: Foldable t => DataCon -> t a -> SDoc -> LintM ()
lintConApp con args app = do
    unarised <- lf_unarised <$> getLintFlags
    when (unarised &&
          not (isUnboxedTupleDataCon con) &&
          length (dataConRuntimeRepStrictness con) /= length args) $ do
      addErrL (text "Constructor applied to incorrect number of arguments:" $$
               text "Application:" <> app)

-- See Note [Linting StgApp]
-- See Note [Typing the STG language]
lintStgAppReps :: Id -> [StgArg] -> LintM ()
lintStgAppReps _fun [] = return ()
lintStgAppReps fun args = do
  lf <- getLintFlags
  let platform = lf_platform lf
      (fun_arg_tys, _res) = splitFunTys (idType fun)
      fun_arg_tys' = map (scaledThing ) fun_arg_tys :: [Type]
      fun_arg_tys_reps, actual_arg_reps :: [Maybe [PrimRep]]
      fun_arg_tys_reps = map typePrimRep_maybe fun_arg_tys'
      actual_arg_reps = map (typePrimRep_maybe . stgArgType) args

      match_args :: [Maybe [PrimRep]] -> [Maybe [PrimRep]] -> LintM ()
      match_args (m_actual_rep:actual_reps_left) (m_expected_rep:expected_reps_left)
        -- Might be wrongly typed as polymorphic. See #21399
        | isNothing m_actual_rep || isNothing m_expected_rep = return ()

        -- Common case, reps are exactly the same
        | m_actual_rep == m_expected_rep
        = match_args actual_reps_left expected_reps_left

        -- Check for void rep which can be either an empty list *or* [VoidRep]
        | isVoidRep actual_rep && isVoidRep expected_rep
        = match_args actual_reps_left expected_reps_left

        -- Some reps are compatible *even* if they are not the same. E.g. IntRep and WordRep.
        -- We check for that here with primRepCompatible
        | and $ zipWith (primRepCompatible platform) actual_rep expected_rep
        = match_args actual_reps_left expected_reps_left

        | otherwise = addErrL $ hang (text "Function type reps and function argument reps missmatched") 2 $
            (text "In application " <> ppr fun <+> ppr args $$
              text "argument rep:" <> ppr actual_rep $$
              text "expected rep:" <> ppr expected_rep $$
              -- text "expected reps:" <> ppr arg_ty_reps $$
              text "unarised?:" <> ppr (lf_unarised lf))
        where
          isVoidRep [] = True
          isVoidRep [VoidRep] = True
          isVoidRep _ = False
          actual_rep = expectJust "match_args m_actual_rep" m_actual_rep
          expected_rep = expectJust "match_args m_expected_rep" m_expected_rep

          -- n_arg_ty_reps = length arg_ty_reps

      match_args _ _ = return () -- Functions are allowed to be over/under applied.

  match_args actual_arg_reps fun_arg_tys_reps

lintAppCbvMarks :: OutputablePass pass
                => GenStgExpr pass -> LintM ()
lintAppCbvMarks e@(StgApp fun args) = do
  lf <- getLintFlags
  when (lf_unarised lf) $ do
    -- A function which expects a unlifted argument as n'th argument
    -- always needs to be applied to n arguments.
    -- See Note [Strict Worker Ids].
    let marks = fromMaybe [] $ idCbvMarks_maybe fun
    when (length (dropWhileEndLE (not . isMarkedCbv) marks) > length args) $ do
      addErrL $ hang (text "Undersatured cbv marked ID in App" <+> ppr e ) 2 $
        (text "marks" <> ppr marks $$
        text "args" <> ppr args $$
        text "arity" <> ppr (idArity fun) $$
        text "join_arity" <> ppr (isJoinId_maybe fun))
lintAppCbvMarks _ = panic "impossible - lintAppCbvMarks"

{-
************************************************************************
*                                                                      *
The Lint monad
*                                                                      *
************************************************************************
-}

newtype LintM a = LintM
    { unLintM :: Module
              -> LintFlags
              -> DiagOpts          -- Diagnostic options
              -> StgPprOpts        -- Pretty-printing options
              -> [LintLocInfo]     -- Locations
              -> IdSet             -- Local vars in scope
              -> Bag SDoc        -- Error messages so far
              -> (a, Bag SDoc)   -- Result and error messages (if any)
    }
    deriving (Functor)

data LintFlags = LintFlags { lf_unarised :: !Bool
                           , lf_platform :: !Platform
                             -- ^ have we run the unariser yet?
                           }

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | LambdaBodyOf [Id]   -- The lambda-binder
  | BodyOfLetRec [Id]   -- One of the binders

dumpLoc :: LintLocInfo -> (SrcSpan, SDoc)
dumpLoc (RhsOf v) =
  (srcLocSpan (getSrcLoc v), text " [RHS of " <> pp_binders [v] <> char ']' )
dumpLoc (LambdaBodyOf bs) =
  (srcLocSpan (getSrcLoc (head bs)), text " [in body of lambda with binders " <> pp_binders bs <> char ']' )

dumpLoc (BodyOfLetRec bs) =
  (srcLocSpan (getSrcLoc (head bs)), text " [in body of letrec with binders " <> pp_binders bs <> char ']' )


pp_binders :: [Id] -> SDoc
pp_binders bs
  = sep (punctuate comma (map pp_binder bs))
  where
    pp_binder b
      = hsep [ppr b, dcolon, ppr (idType b)]

initL :: Platform -> DiagOpts -> Module -> Bool -> StgPprOpts -> IdSet -> LintM a -> Maybe SDoc
initL platform diag_opts this_mod unarised opts locals (LintM m) = do
  let (_, errs) = m this_mod (LintFlags unarised platform) diag_opts opts [] locals emptyBag
  if isEmptyBag errs then
      Nothing
  else
      Just (vcat (punctuate blankLine (bagToList errs)))

instance Applicative LintM where
      pure a = LintM $ \_mod _lf _df _opts _loc _scope errs -> (a, errs)
      (<*>) = ap
      (*>)  = thenL_

instance Monad LintM where
    (>>=) = thenL
    (>>)  = (*>)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k = LintM $ \mod lf diag_opts opts loc scope errs
  -> case unLintM m mod lf diag_opts opts loc scope errs of
      (r, errs') -> unLintM (k r) mod lf diag_opts opts loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k = LintM $ \mod lf diag_opts opts loc scope errs
  -> case unLintM m mod lf diag_opts opts loc scope errs of
      (_, errs') -> unLintM k mod lf diag_opts opts loc scope errs'

checkL :: Bool -> SDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = addErrL msg

-- Case alts shouldn't have unboxed sum, unboxed tuple, or void binders.
checkPostUnariseBndr :: Id -> LintM ()
checkPostUnariseBndr bndr = do
    lf <- getLintFlags
    when (lf_unarised lf) $
      forM_ (checkPostUnariseId bndr) $ \unexpected ->
        addErrL $
          text "After unarisation, binder " <>
          ppr bndr <> text " has " <> text unexpected <> text " type " <>
          ppr (idType bndr)

-- Arguments shouldn't have sum, tuple, or void types.
checkPostUnariseConArg :: StgArg -> LintM ()
checkPostUnariseConArg arg = case arg of
    StgLitArg _ ->
      return ()
    StgVarArg id -> do
      lf <- getLintFlags
      when (lf_unarised lf) $
        forM_ (checkPostUnariseId id) $ \unexpected ->
          addErrL $
            text "After unarisation, arg " <>
            ppr id <> text " has " <> text unexpected <> text " type " <>
            ppr (idType id)

-- Post-unarisation args and case alt binders should not have unboxed tuple,
-- unboxed sum, or void types. Return what the binder is if it is one of these.
checkPostUnariseId :: Id -> Maybe String
checkPostUnariseId id =
    let
      id_ty = idType id
      is_sum, is_tuple, is_void :: Maybe String
      is_sum = guard (isUnboxedSumType id_ty) >> return "unboxed sum"
      is_tuple = guard (isUnboxedTupleType id_ty) >> return "unboxed tuple"
      is_void = guard (isZeroBitTy id_ty) >> return "void"
    in
      is_sum <|> is_tuple <|> is_void

addErrL :: SDoc -> LintM ()
addErrL msg = LintM $ \_mod _lf df _opts loc _scope errs -> ((), addErr df errs msg loc)

addErr :: DiagOpts -> Bag SDoc -> SDoc -> [LintLocInfo] -> Bag SDoc
addErr diag_opts errs_so_far msg locs
  = errs_so_far `snocBag` mk_msg locs
  where
    mk_msg (loc:_) = let (l,hdr) = dumpLoc loc
                     in  mkLocMessage (Err.mkMCDiagnostic diag_opts WarningWithoutFlag)
                                      l (hdr $$ msg)
    mk_msg []      = msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m = LintM $ \mod lf diag_opts opts loc scope errs
   -> unLintM m mod lf diag_opts opts (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m = LintM $ \mod lf diag_opts opts loc scope errs
 -> let
        new_set = mkVarSet ids
    in unLintM m mod lf diag_opts opts loc (scope `unionVarSet` new_set) errs

getLintFlags :: LintM LintFlags
getLintFlags = LintM $ \_mod lf _df _opts _loc _scope errs -> (lf, errs)

getStgPprOpts :: LintM StgPprOpts
getStgPprOpts = LintM $ \_mod _lf _df opts _loc _scope errs -> (opts, errs)

checkInScope :: Id -> LintM ()
checkInScope id = LintM $ \mod _lf diag_opts _opts loc scope errs
 -> if nameIsLocalOrFrom mod (idName id) && not (id `elemVarSet` scope) then
        ((), addErr diag_opts errs (hsep [ppr id, dcolon, ppr (idType id),
                                    text "is out of scope"]) loc)
    else
        ((), errs)

mkUnliftedTyMsg :: OutputablePass a => StgPprOpts -> Id -> GenStgRhs a -> SDoc
mkUnliftedTyMsg opts binder rhs
  = (text "Let(rec) binder" <+> quotes (ppr binder) <+>
     text "has unlifted type" <+> quotes (ppr (idType binder)))
    $$
    (text "RHS:" <+> pprStgRhs opts rhs)
