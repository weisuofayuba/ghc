{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.Tc.Solver.Interact (
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds,  -- Solves Cts
  ) where

#include "HsVersions.h"

import GHC.Prelude
import GHC.Types.Basic ( SwapFlag(..), isSwapped,
                         infinity, IntWithInf, intGtLimit )
import GHC.Tc.Solver.Canonical
import GHC.Tc.Solver.Flatten
import GHC.Tc.Utils.Unify( canSolveByUnification )
import GHC.Types.Var.Set
import GHC.Core.Type as Type
import GHC.Core.Coercion        ( BlockSubstFlag(..) )
import GHC.Core.InstEnv         ( DFunInstType )
import GHC.Core.Coercion.Axiom  ( sfInteractTop, sfInteractInert )

import GHC.Types.Var
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names ( coercibleTyConKey,
                   heqTyConKey, eqTyConKey, ipClassKey )
import GHC.Core.Coercion.Axiom ( TypeEqn, CoAxiom(..), CoAxBranch(..), fromBranches )
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Instance.Family
import GHC.Tc.Instance.Class( InstanceWhat(..), safeOverlap )
import GHC.Core.FamInstEnv
import GHC.Core.Unify ( tcUnifyTyWithTFs, ruleMatchTyKiX )

import GHC.Tc.Types.Evidence
import GHC.Utils.Outputable

import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.Monad
import GHC.Data.Bag
import GHC.Utils.Monad ( concatMapM, foldlM )

import GHC.Core
import Data.List( partition, deleteFirstsBy )
import GHC.Types.SrcLoc
import GHC.Types.Var.Env

import Control.Monad
import GHC.Data.Maybe( isJust )
import GHC.Data.Pair (Pair(..))
import GHC.Types.Unique( hasKey )
import GHC.Driver.Session
import GHC.Utils.Misc
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

{-
**********************************************************************
*                                                                    *
*                      Main Interaction Solver                       *
*                                                                    *
**********************************************************************

Note [Basic Simplifier Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Pick an element from the WorkList if there exists one with depth
   less than our context-stack depth.

2. Run it down the 'stage' pipeline. Stages are:
      - canonicalization
      - inert reactions
      - spontaneous reactions
      - top-level interactions
   Each stage returns a StopOrContinue and may have sideffected
   the inerts or worklist.

   The threading of the stages is as follows:
      - If (Stop) is returned by a stage then we start again from Step 1.
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to
        the next stage in the pipeline.
4. If the element has survived (i.e. ContinueWith x) the last stage
   then we add him in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack
depth and will simply fail.

Note [Unflatten after solving the simple wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We unflatten after solving the wc_simples of an implication, and before attempting
to float. This means that

 * The fsk/fmv flatten-skolems only survive during solveSimples.  We don't
   need to worry about them across successive passes over the constraint tree.
   (E.g. we don't need the old ic_fsk field of an implication.

 * When floating an equality outwards, we don't need to worry about floating its
   associated flattening constraints.

 * Another tricky case becomes easy: #4935
       type instance F True a b = a
       type instance F False a b = b

       [w] F c a b ~ gamma
       (c ~ True) => a ~ gamma
       (c ~ False) => b ~ gamma

   Obviously this is soluble with gamma := F c a b, and unflattening
   will do exactly that after solving the simple constraints and before
   attempting the implications.  Before, when we were not unflattening,
   we had to push Wanted funeqs in as new givens.  Yuk!

   Another example that becomes easy: indexed_types/should_fail/T7786
      [W] BuriedUnder sub k Empty ~ fsk
      [W] Intersect fsk inv ~ s
      [w] xxx[1] ~ s
      [W] forall[2] . (xxx[1] ~ Empty)
                   => Intersect (BuriedUnder sub k Empty) inv ~ Empty

Note [Running plugins on unflattened wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is an annoying mismatch between solveSimpleGivens and
solveSimpleWanteds, because the latter needs to fiddle with the inert
set, unflatten and zonk the wanteds.  It passes the zonked wanteds
to runTcPluginsWanteds, which produces a replacement set of wanteds,
some additional insolubles and a flag indicating whether to go round
the loop again.  If so, prepareInertsForImplications is used to remove
the previous wanteds (which will still be in the inert set).  Note
that prepareInertsForImplications will discard the insolubles, so we
must keep track of them separately.
-}

solveSimpleGivens :: [Ct] -> TcS ()
solveSimpleGivens givens
  | null givens  -- Shortcut for common case
  = return ()
  | otherwise
  = do { traceTcS "solveSimpleGivens {" (ppr givens)
       ; go givens
       ; traceTcS "End solveSimpleGivens }" empty }
  where
    go givens = do { solveSimples (listToBag givens)
                   ; new_givens <- runTcPluginsGiven
                   ; when (notNull new_givens) $
                     go new_givens }

solveSimpleWanteds :: Cts -> TcS WantedConstraints
-- NB: 'simples' may contain /derived/ equalities, floated
--     out from a nested implication. So don't discard deriveds!
-- The result is not necessarily zonked
solveSimpleWanteds simples
  = do { traceTcS "solveSimpleWanteds {" (ppr simples)
       ; dflags <- getDynFlags
       ; (n,wc) <- go 1 (solverIterations dflags) (emptyWC { wc_simple = simples })
       ; traceTcS "solveSimpleWanteds end }" $
             vcat [ text "iterations =" <+> ppr n
                  , text "residual =" <+> ppr wc ]
       ; return wc }
  where
    go :: Int -> IntWithInf -> WantedConstraints -> TcS (Int, WantedConstraints)
    go n limit wc
      | n `intGtLimit` limit
      = failTcS (hang (text "solveSimpleWanteds: too many iterations"
                       <+> parens (text "limit =" <+> ppr limit))
                    2 (vcat [ text "Set limit with -fconstraint-solver-iterations=n; n=0 for no limit"
                            , text "Simples =" <+> ppr simples
                            , text "WC ="      <+> ppr wc ]))

     | isEmptyBag (wc_simple wc)
     = return (n,wc)

     | otherwise
     = do { -- Solve
            (unif_count, wc1) <- solve_simple_wanteds wc

            -- Run plugins
          ; (rerun_plugin, wc2) <- runTcPluginsWanted wc1
             -- See Note [Running plugins on unflattened wanteds]

          ; if unif_count == 0 && not rerun_plugin
            then return (n, wc2)             -- Done
            else do { traceTcS "solveSimple going round again:" $
                      ppr unif_count $$ ppr rerun_plugin
                    ; go (n+1) limit wc2 } }      -- Loop


solve_simple_wanteds :: WantedConstraints -> TcS (Int, WantedConstraints)
-- Try solving these constraints
-- Affects the unification state (of course) but not the inert set
-- The result is not necessarily zonked
solve_simple_wanteds (WC { wc_simple = simples1, wc_impl = implics1, wc_holes = holes })
  = nestTcS $
    do { solveSimples simples1
       ; (implics2, tv_eqs, fun_eqs, others) <- getUnsolvedInerts
       ; (unif_count, unflattened_eqs) <- reportUnifications $
                                          unflattenWanteds tv_eqs fun_eqs
            -- See Note [Unflatten after solving the simple wanteds]
       ; return ( unif_count
                , WC { wc_simple = others `andCts` unflattened_eqs
                     , wc_impl   = implics1 `unionBags` implics2
                     , wc_holes  = holes }) }

{- Note [The solveSimpleWanteds loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a bunch of simple constraints is done in a loop,
(the 'go' loop of 'solveSimpleWanteds'):
  1. Try to solve them; unflattening may lead to improvement that
     was not exploitable during solving
  2. Try the plugin
  3. If step 1 did improvement during unflattening; or if the plugin
     wants to run again, go back to step 1

Non-obviously, improvement can also take place during
the unflattening that takes place in step (1). See GHC.Tc.Solver.Flatten,
See Note [Unflattening can force the solver to iterate]
-}

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveSimples :: Cts -> TcS ()
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-implications
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { updWorkListTcS (\wl -> foldr extendWorkListCt wl cts)
       ; solve_loop }
  where
    solve_loop
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem
           ; case sel of
              Nothing -> return ()
              Just ct -> do { runSolverPipeline thePipeline ct
                            ; solve_loop } }

-- | Extract the (inert) givens and invoke the plugins on them.
-- Remove solved givens from the inert set and emit insolubles, but
-- return new work produced so that 'solveSimpleGivens' can feed it back
-- into the main solver.
runTcPluginsGiven :: TcS [Ct]
runTcPluginsGiven
  = do { plugins <- getTcPlugins
       ; if null plugins then return [] else
    do { givens <- getInertGivens
       ; if null givens then return [] else
    do { p <- runTcPlugins plugins (givens,[],[])
       ; let (solved_givens, _, _) = pluginSolvedCts p
             insols                = pluginBadCts p
       ; updInertCans (removeInertCts solved_givens)
       ; updInertIrreds (\irreds -> extendCtsList irreds insols)
       ; return (pluginNewCts p) } } }

-- | Given a bag of (flattened, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: WantedConstraints -> TcS (Bool, WantedConstraints)
runTcPluginsWanted wc@(WC { wc_simple = simples1 })
  | isEmptyBag simples1
  = return (False, wc)
  | otherwise
  = do { plugins <- getTcPlugins
       ; if null plugins then return (False, wc) else

    do { given <- getInertGivens
       ; simples1 <- zonkSimples simples1    -- Plugin requires zonked inputs
       ; let (wanted, derived) = partition isWantedCt (bagToList simples1)
       ; p <- runTcPlugins plugins (given, derived, wanted)
       ; let (_, _,                solved_wanted)   = pluginSolvedCts p
             (_, unsolved_derived, unsolved_wanted) = pluginInputCts p
             new_wanted                             = pluginNewCts p
             insols                                 = pluginBadCts p

-- SLPJ: I'm deeply suspicious of this
--       ; updInertCans (removeInertCts $ solved_givens ++ solved_deriveds)

       ; mapM_ setEv solved_wanted
       ; return ( notNull (pluginNewCts p)
                , wc { wc_simple = listToBag new_wanted       `andCts`
                                   listToBag unsolved_wanted  `andCts`
                                   listToBag unsolved_derived `andCts`
                                   listToBag insols } ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest ev
      _ -> panic "runTcPluginsWanted.setEv: attempt to solve non-wanted!"

-- | A triple of (given, derived, wanted) constraints to pass to plugins
type SplitCts  = ([Ct], [Ct], [Ct])

-- | A solved triple of constraints, with evidence for wanteds
type SolvedCts = ([Ct], [Ct], [(EvTerm,Ct)])

-- | Represents collections of constraints generated by typechecker
-- plugins
data TcPluginProgress = TcPluginProgress
    { pluginInputCts  :: SplitCts
      -- ^ Original inputs to the plugins with solved/bad constraints
      -- removed, but otherwise unmodified
    , pluginSolvedCts :: SolvedCts
      -- ^ Constraints solved by plugins
    , pluginBadCts    :: [Ct]
      -- ^ Constraints reported as insoluble by plugins
    , pluginNewCts    :: [Ct]
      -- ^ New constraints emitted by plugins
    }

getTcPlugins :: TcS [TcPluginSolver]
getTcPlugins = do { tcg_env <- getGblEnv; return (tcg_tc_plugins tcg_env) }

-- | Starting from a triple of (given, derived, wanted) constraints,
-- invoke each of the typechecker plugins in turn and return
--
--  * the remaining unmodified constraints,
--  * constraints that have been solved,
--  * constraints that are insoluble, and
--  * new work.
--
-- Note that new work generated by one plugin will not be seen by
-- other plugins on this pass (but the main constraint solver will be
-- re-invoked and they will see it later).  There is no check that new
-- work differs from the original constraints supplied to the plugin:
-- the plugin itself should perform this check if necessary.
runTcPlugins :: [TcPluginSolver] -> SplitCts -> TcS TcPluginProgress
runTcPlugins plugins all_cts
  = foldM do_plugin initialProgress plugins
  where
    do_plugin :: TcPluginProgress -> TcPluginSolver -> TcS TcPluginProgress
    do_plugin p solver = do
        result <- runTcPluginTcS (uncurry3 solver (pluginInputCts p))
        return $ progress p result

    progress :: TcPluginProgress -> TcPluginResult -> TcPluginProgress
    progress p (TcPluginContradiction bad_cts) =
       p { pluginInputCts = discard bad_cts (pluginInputCts p)
         , pluginBadCts   = bad_cts ++ pluginBadCts p
         }
    progress p (TcPluginOk solved_cts new_cts) =
      p { pluginInputCts  = discard (map snd solved_cts) (pluginInputCts p)
        , pluginSolvedCts = add solved_cts (pluginSolvedCts p)
        , pluginNewCts    = new_cts ++ pluginNewCts p
        }

    initialProgress = TcPluginProgress all_cts ([], [], []) [] []

    discard :: [Ct] -> SplitCts -> SplitCts
    discard cts (xs, ys, zs) =
        (xs `without` cts, ys `without` cts, zs `without` cts)

    without :: [Ct] -> [Ct] -> [Ct]
    without = deleteFirstsBy eqCt

    eqCt :: Ct -> Ct -> Bool
    eqCt c c' = ctFlavour c == ctFlavour c'
             && ctPred c `tcEqType` ctPred c'

    add :: [(EvTerm,Ct)] -> SolvedCts -> SolvedCts
    add xs scs = foldl' addOne scs xs

    addOne :: SolvedCts -> (EvTerm,Ct) -> SolvedCts
    addOne (givens, deriveds, wanteds) (ev,ct) = case ctEvidence ct of
      CtGiven  {} -> (ct:givens, deriveds, wanteds)
      CtDerived{} -> (givens, ct:deriveds, wanteds)
      CtWanted {} -> (givens, deriveds, (ev,ct):wanteds)


type WorkItem = Ct
type SimplifierStage = WorkItem -> TcS (StopOrContinue Ct)

runSolverPipeline :: [(String,SimplifierStage)] -- The pipeline
                  -> WorkItem                   -- The work item
                  -> TcS ()
-- Run this item down the pipeline, leaving behind new work and inerts
runSolverPipeline pipeline workItem
  = do { wl <- getWorkList
       ; inerts <- getTcSInerts
       ; tclevel <- getTcLevel
       ; traceTcS "----------------------------- " empty
       ; traceTcS "Start solver pipeline {" $
                  vcat [ text "tclevel =" <+> ppr tclevel
                       , text "work item =" <+> ppr workItem
                       , text "inerts =" <+> ppr inerts
                       , text "rest of worklist =" <+> ppr wl ]

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; final_res  <- run_pipeline pipeline (ContinueWith workItem)

       ; case final_res of
           Stop ev s       -> do { traceFireTcS ev s
                                 ; traceTcS "End solver pipeline (discharged) }" empty
                                 ; return () }
           ContinueWith ct -> do { addInertCan ct
                                 ; traceFireTcS (ctEvidence ct) (text "Kept as inert")
                                 ; traceTcS "End solver pipeline (kept as inert) }" $
                                            (text "final_item =" <+> ppr ct) }
       }
  where run_pipeline :: [(String,SimplifierStage)] -> StopOrContinue Ct
                     -> TcS (StopOrContinue Ct)
        run_pipeline [] res        = return res
        run_pipeline _ (Stop ev s) = return (Stop ev s)
        run_pipeline ((stg_name,stg):stgs) (ContinueWith ct)
          = do { traceTcS ("runStage " ++ stg_name ++ " {")
                          (text "workitem   = " <+> ppr ct)
               ; res <- stg ct
               ; traceTcS ("end stage " ++ stg_name ++ " }") empty
               ; run_pipeline stgs res }

{-
Example 1:
  Inert:   {c ~ d, F a ~ t, b ~ Int, a ~ ty} (all given)
  Reagent: a ~ [b] (given)

React with (c~d)     ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~ t) ==> IR (ContinueWith (a~[b]))  False   [F [b] ~ t]
React with (b ~ Int) ==> IR (ContinueWith (a~[Int]) True    []

Example 2:
  Inert:  {c ~w d, F a ~g t, b ~w Int, a ~w ty}
  Reagent: a ~w [b]

React with (c ~w d)   ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~g t) ==> IR (ContinueWith (a~[b]))  True    []    (can't rewrite given with wanted!)
etc.

Example 3:
  Inert:  {a ~ Int, F Int ~ b} (given)
  Reagent: F a ~ b (wanted)

React with (a ~ Int)   ==> IR (ContinueWith (F Int ~ b)) True []
React with (F Int ~ b) ==> IR Stop True []    -- after substituting we re-canonicalize and get nothing
-}

thePipeline :: [(String,SimplifierStage)]
thePipeline = [ ("canonicalization",        GHC.Tc.Solver.Canonical.canonicalize)
              , ("interact with inerts",    interactWithInertsStage)
              , ("top-level reactions",     topReactionsStage) ]

{-
*********************************************************************************
*                                                                               *
                       The interact-with-inert Stage
*                                                                               *
*********************************************************************************

Note [The Solver Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We always add Givens first.  So you might think that the solver has
the invariant

   If the work-item is Given,
   then the inert item must Given

But this isn't quite true.  Suppose we have,
    c1: [W] beta ~ [alpha], c2 : [W] blah, c3 :[W] alpha ~ Int
After processing the first two, we get
     c1: [G] beta ~ [alpha], c2 : [W] blah
Now, c3 does not interact with the given c1, so when we spontaneously
solve c3, we must re-react it with the inert set.  So we can attempt a
reaction between inert c2 [W] and work-item c3 [G].

It *is* true that [Solver Invariant]
   If the work-item is Given,
   AND there is a reaction
   then the inert item must Given
or, equivalently,
   If the work-item is Given,
   and the inert item is Wanted/Derived
   then there is no reaction
-}

-- Interaction result of  WorkItem <~> Ct

interactWithInertsStage :: WorkItem -> TcS (StopOrContinue Ct)
-- Precondition: if the workitem is a CTyEqCan then it will not be able to
-- react with anything at this stage.

interactWithInertsStage wi
  = do { inerts <- getTcSInerts
       ; let ics = inert_cans inerts
       ; case wi of
             CTyEqCan  {} -> interactTyVarEq ics wi
             CFunEqCan {} -> interactFunEq   ics wi
             CIrredCan {} -> interactIrred   ics wi
             CDictCan  {} -> interactDict    ics wi
             _ -> pprPanic "interactWithInerts" (ppr wi) }
                -- CNonCanonical have been canonicalised

data InteractResult
   = KeepInert   -- Keep the inert item, and solve the work item from it
                 -- (if the latter is Wanted; just discard it if not)
   | KeepWork    -- Keep the work item, and solve the inert item from it

   | KeepBoth    -- See Note [KeepBoth]

instance Outputable InteractResult where
  ppr KeepBoth  = text "keep both"
  ppr KeepInert = text "keep inert"
  ppr KeepWork  = text "keep work-item"

{- Note [KeepBoth]
~~~~~~~~~~~~~~~~~~
Consider
   Inert:     [WD] C ty1 ty2
   Work item: [D]  C ty1 ty2

Here we can simply drop the work item. But what about
   Inert:     [W] C ty1 ty2
   Work item: [D] C ty1 ty2

Here we /cannot/ drop the work item, becuase we lose the [D] form, and
that is essential for e.g. fundeps, see isImprovable.  We could zap
the inert item to [WD], but the simplest thing to do is simply to keep
both. (They probably started as [WD] and got split; this is relatively
rare and it doesn't seem worth trying to put them back together again.)
-}

solveOneFromTheOther :: CtEvidence  -- Inert
                     -> CtEvidence  -- WorkItem
                     -> TcS InteractResult
-- Precondition:
-- * inert and work item represent evidence for the /same/ predicate
--
-- We can always solve one from the other: even if both are wanted,
-- although we don't rewrite wanteds with wanteds, we can combine
-- two wanteds into one by solving one from the other

solveOneFromTheOther ev_i ev_w
  | CtDerived {} <- ev_w         -- Work item is Derived
  = case ev_i of
      CtWanted { ctev_nosh = WOnly } -> return KeepBoth
      _                              -> return KeepInert

  | CtDerived {} <- ev_i         -- Inert item is Derived
  = case ev_w of
      CtWanted { ctev_nosh = WOnly } -> return KeepBoth
      _                              -> return KeepWork
              -- The ev_w is inert wrt earlier inert-set items,
              -- so it's safe to continue on from this point

  -- After this, neither ev_i or ev_w are Derived
  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve (ctEvLoc ev_i) loc_w
  = -- inert must be Given
    do { traceTcS "prohibitedClassSolve1" (ppr ev_i $$ ppr ev_w)
       ; return KeepWork }

  | CtWanted { ctev_nosh = nosh_w } <- ev_w
       -- Inert is Given or Wanted
  = case ev_i of
      CtWanted { ctev_nosh = WOnly }
          | WDeriv <- nosh_w -> return KeepWork
      _                      -> return KeepInert
      -- Consider work  item [WD] C ty1 ty2
      --          inert item [W]  C ty1 ty2
      -- Then we must keep the work item.  But if the
      -- work item was       [W]  C ty1 ty2
      -- then we are free to discard the work item in favour of inert
      -- Remember, no Deriveds at this point

  -- From here on the work-item is Given

  | CtWanted { ctev_loc = loc_i } <- ev_i
  , prohibitedSuperClassSolve (ctEvLoc ev_w) loc_i
  = do { traceTcS "prohibitedClassSolve2" (ppr ev_i $$ ppr ev_w)
       ; return KeepInert }      -- Just discard the un-usable Given
                                 -- This never actually happens because
                                 -- Givens get processed first

  | CtWanted {} <- ev_i
  = return KeepWork

  -- From here on both are Given
  -- See Note [Replacement vs keeping]

  | lvl_i == lvl_w
  = do { ev_binds_var <- getTcEvBindsVar
       ; binds <- getTcEvBindsMap ev_binds_var
       ; return (same_level_strategy binds) }

  | otherwise   -- Both are Given, levels differ
  = return different_level_strategy
  where
     pred  = ctEvPred ev_i
     loc_i = ctEvLoc ev_i
     loc_w = ctEvLoc ev_w
     lvl_i = ctLocLevel loc_i
     lvl_w = ctLocLevel loc_w
     ev_id_i = ctEvEvId ev_i
     ev_id_w = ctEvEvId ev_w

     different_level_strategy  -- Both Given
       | isIPLikePred pred = if lvl_w > lvl_i then KeepWork  else KeepInert
       | otherwise         = if lvl_w > lvl_i then KeepInert else KeepWork
       -- See Note [Replacement vs keeping] (the different-level bullet)
       -- For the isIPLikePred case see Note [Shadowing of Implicit Parameters]

     same_level_strategy binds -- Both Given
       | GivenOrigin (InstSC s_i) <- ctLocOrigin loc_i
       = case ctLocOrigin loc_w of
            GivenOrigin (InstSC s_w) | s_w < s_i -> KeepWork
                                     | otherwise -> KeepInert
            _                                    -> KeepWork

       | GivenOrigin (InstSC {}) <- ctLocOrigin loc_w
       = KeepInert

       | has_binding binds ev_id_w
       , not (has_binding binds ev_id_i)
       , not (ev_id_i `elemVarSet` findNeededEvVars binds (unitVarSet ev_id_w))
       = KeepWork

       | otherwise
       = KeepInert

     has_binding binds ev_id = isJust (lookupEvBind binds ev_id)

{-
Note [Replacement vs keeping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have two Given constraints both of type (C tys), say, which should
we keep?  More subtle than you might think!

  * Constraints come from different levels (different_level_strategy)

      - For implicit parameters we want to keep the innermost (deepest)
        one, so that it overrides the outer one.
        See Note [Shadowing of Implicit Parameters]

      - For everything else, we want to keep the outermost one.  Reason: that
        makes it more likely that the inner one will turn out to be unused,
        and can be reported as redundant.  See Note [Tracking redundant constraints]
        in GHC.Tc.Solver.

        It transpires that using the outermost one is responsible for an
        8% performance improvement in nofib cryptarithm2, compared to
        just rolling the dice.  I didn't investigate why.

  * Constraints coming from the same level (i.e. same implication)

       (a) Always get rid of InstSC ones if possible, since they are less
           useful for solving.  If both are InstSC, choose the one with
           the smallest TypeSize
           See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance

       (b) Keep the one that has a non-trivial evidence binding.
              Example:  f :: (Eq a, Ord a) => blah
              then we may find [G] d3 :: Eq a
                               [G] d2 :: Eq a
                with bindings  d3 = sc_sel (d1::Ord a)
            We want to discard d2 in favour of the superclass selection from
            the Ord dictionary.
            Why? See Note [Tracking redundant constraints] in GHC.Tc.Solver again.

       (c) But don't do (b) if the evidence binding depends transitively on the
           one without a binding.  Example (with RecursiveSuperClasses)
              class C a => D a
              class D a => C a
           Inert:     d1 :: C a, d2 :: D a
           Binds:     d3 = sc_sel d2, d2 = sc_sel d1
           Work item: d3 :: C a
           Then it'd be ridiculous to replace d1 with d3 in the inert set!
           Hence the findNeedEvVars test.  See #14774.

  * Finally, when there is still a choice, use KeepInert rather than
    KeepWork, for two reasons:
      - to avoid unnecessary munging of the inert set.
      - to cut off superclass loops; see Note [Superclass loops] in GHC.Tc.Solver.Canonical

Doing the depth-check for implicit parameters, rather than making the work item
always override, is important.  Consider

    data T a where { T1 :: (?x::Int) => T Int; T2 :: T a }

    f :: (?x::a) => T a -> Int
    f T1 = ?x
    f T2 = 3

We have a [G] (?x::a) in the inert set, and at the pattern match on T1 we add
two new givens in the work-list:  [G] (?x::Int)
                                  [G] (a ~ Int)
Now consider these steps
  - process a~Int, kicking out (?x::a)
  - process (?x::Int), the inner given, adding to inert set
  - process (?x::a), the outer given, overriding the inner given
Wrong!  The depth-check ensures that the inner implicit parameter wins.
(Actually I think that the order in which the work-list is processed means
that this chain of events won't happen, but that's very fragile.)

*********************************************************************************
*                                                                               *
                   interactIrred
*                                                                               *
*********************************************************************************

Note [Multiple matching irreds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that it's impossible to have multiple irreds all match the
work item; after all, interactIrred looks for matches and solves one from the
other. However, note that interacting insoluble, non-droppable irreds does not
do this matching. We thus might end up with several insoluble, non-droppable,
matching irreds in the inert set. When another irred comes along that we have
not yet labeled insoluble, we can find multiple matches. These multiple matches
cause no harm, but it would be wrong to ASSERT that they aren't there (as we
once had done). This problem can be tickled by typecheck/should_compile/holes.

-}

-- Two pieces of irreducible evidence: if their types are *exactly identical*
-- we can rewrite them. We can never improve using this:
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not
-- mean that (ty1 ~ ty2)
interactIrred :: InertCans -> Ct -> TcS (StopOrContinue Ct)

interactIrred inerts workItem@(CIrredCan { cc_ev = ev_w, cc_status = status })
  | InsolubleCIS <- status
               -- For insolubles, don't allow the constraint to be dropped
               -- which can happen with solveOneFromTheOther, so that
               -- we get distinct error messages with -fdefer-type-errors
               -- See Note [Do not add duplicate derived insolubles]
  , not (isDroppableCt workItem)
  = continueWith workItem

  | let (matching_irreds, others) = findMatchingIrreds (inert_irreds inerts) ev_w
  , ((ct_i, swap) : _rest) <- bagToList matching_irreds
        -- See Note [Multiple matching irreds]
  , let ev_i = ctEvidence ct_i
  = do { what_next <- solveOneFromTheOther ev_i ev_w
       ; traceTcS "iteractIrred" (ppr workItem $$ ppr what_next $$ ppr ct_i)
       ; case what_next of
            KeepBoth  -> continueWith workItem
            KeepInert -> do { setEvBindIfWanted ev_w (swap_me swap ev_i)
                            ; return (Stop ev_w (text "Irred equal" <+> parens (ppr what_next))) }
            KeepWork ->  do { setEvBindIfWanted ev_i (swap_me swap ev_w)
                            ; updInertIrreds (\_ -> others)
                            ; continueWith workItem } }

  | otherwise
  = continueWith workItem

  where
    swap_me :: SwapFlag -> CtEvidence -> EvTerm
    swap_me swap ev
      = case swap of
           NotSwapped -> ctEvTerm ev
           IsSwapped  -> evCoercion (mkTcSymCo (evTermCoercion (ctEvTerm ev)))

interactIrred _ wi = pprPanic "interactIrred" (ppr wi)

findMatchingIrreds :: Cts -> CtEvidence -> (Bag (Ct, SwapFlag), Bag Ct)
findMatchingIrreds irreds ev
  | EqPred eq_rel1 lty1 rty1 <- classifyPredType pred
    -- See Note [Solving irreducible equalities]
  = partitionBagWith (match_eq eq_rel1 lty1 rty1) irreds
  | otherwise
  = partitionBagWith match_non_eq irreds
  where
    pred = ctEvPred ev
    match_non_eq ct
      | ctPred ct `tcEqTypeNoKindCheck` pred = Left (ct, NotSwapped)
      | otherwise                            = Right ct

    match_eq eq_rel1 lty1 rty1 ct
      | EqPred eq_rel2 lty2 rty2 <- classifyPredType (ctPred ct)
      , eq_rel1 == eq_rel2
      , Just swap <- match_eq_help lty1 rty1 lty2 rty2
      = Left (ct, swap)
      | otherwise
      = Right ct

    match_eq_help lty1 rty1 lty2 rty2
      | lty1 `tcEqTypeNoKindCheck` lty2, rty1 `tcEqTypeNoKindCheck` rty2
      = Just NotSwapped
      | lty1 `tcEqTypeNoKindCheck` rty2, rty1 `tcEqTypeNoKindCheck` lty2
      = Just IsSwapped
      | otherwise
      = Nothing

{- Note [Solving irreducible equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14333)
  [G] a b ~R# c d
  [W] c d ~R# a b
Clearly we should be able to solve this! Even though the constraints are
not decomposable. We solve this when looking up the work-item in the
irreducible constraints to look for an identical one.  When doing this
lookup, findMatchingIrreds spots the equality case, and matches either
way around. It has to return a swap-flag so we can generate evidence
that is the right way round too.

Note [Do not add duplicate derived insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we *must* add an insoluble (Int ~ Bool) even if there is
one such there already, because they may come from distinct call
sites.  Not only do we want an error message for each, but with
-fdefer-type-errors we must generate evidence for each.  But for
*derived* insolubles, we only want to report each one once.  Why?

(a) A constraint (C r s t) where r -> s, say, may generate the same fundep
    equality many times, as the original constraint is successively rewritten.

(b) Ditto the successive iterations of the main solver itself, as it traverses
    the constraint tree. See example below.

Also for *given* insolubles we may get repeated errors, as we
repeatedly traverse the constraint tree.  These are relatively rare
anyway, so removing duplicates seems ok.  (Alternatively we could take
the SrcLoc into account.)

Note that the test does not need to be particularly efficient because
it is only used if the program has a type error anyway.

Example of (b): assume a top-level class and instance declaration:

  class D a b | a -> b
  instance D [a] [a]

Assume we have started with an implication:

  forall c. Eq c => { wc_simple = D [c] c [W] }

which we have simplified to:

  forall c. Eq c => { wc_simple = D [c] c [W]
                                  (c ~ [c]) [D] }

For some reason, e.g. because we floated an equality somewhere else,
we might try to re-solve this implication. If we do not do a
dropDerivedWC, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_simple   = D [c] c [W]
               (c ~ [c]) [D], (c ~ [c]) [D]
-}

{-
*********************************************************************************
*                                                                               *
                   interactDict
*                                                                               *
*********************************************************************************

Note [Shortcut solving]
~~~~~~~~~~~~~~~~~~~~~~~
When we interact a [W] constraint with a [G] constraint that solves it, there is
a possibility that we could produce better code if instead we solved from a
top-level instance declaration (See #12791, #5835). For example:

    class M a b where m :: a -> b

    type C a b = (Num a, M a b)

    f :: C Int b => b -> Int -> Int
    f _ x = x + 1

The body of `f` requires a [W] `Num Int` instance. We could solve this
constraint from the givens because we have `C Int b` and that provides us a
solution for `Num Int`. This would let us produce core like the following
(with -O2):

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) ($d(%,%) :: C Int b) _ (eta1 :: Int) ->
        + @ Int
          (GHC.Classes.$p1(%,%) @ (Num Int) @ (M Int b) $d(%,%))
          eta1
          A.f1

This is bad! We could do /much/ better if we solved [W] `Num Int` directly
from the instance that we have in scope:

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) _ _ (x :: Int) ->
        case x of { GHC.Types.I# x1 -> GHC.Types.I# (GHC.Prim.+# x1 1#) }

** NB: It is important to emphasize that all this is purely an optimization:
** exactly the same programs should typecheck with or without this
** procedure.

Solving fully
~~~~~~~~~~~~~
There is a reason why the solver does not simply try to solve such
constraints with top-level instances. If the solver finds a relevant
instance declaration in scope, that instance may require a context
that can't be solved for. A good example of this is:

    f :: Ord [a] => ...
    f x = ..Need Eq [a]...

If we have instance `Eq a => Eq [a]` in scope and we tried to use it, we would
be left with the obligation to solve the constraint Eq a, which we cannot. So we
must be conservative in our attempt to use an instance declaration to solve the
[W] constraint we're interested in.

Our rule is that we try to solve all of the instance's subgoals
recursively all at once. Precisely: We only attempt to solve
constraints of the form `C1, ... Cm => C t1 ... t n`, where all the Ci
are themselves class constraints of the form `C1', ... Cm' => C' t1'
... tn'` and we only succeed if the entire tree of constraints is
solvable from instances.

An example that succeeds:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [Int] b => b -> Bool
    f x = m x == []

We solve for `Eq [Int]`, which requires `Eq Int`, which we also have. This
produces the following core:

    f :: forall b. C [Int] b => b -> Bool
    f = \ (@ b) ($dC :: C [Int] b) (x :: b) ->
        GHC.Classes.$fEq[]_$s$c==
          (m @ [Int] @ b $dC x) (GHC.Types.[] @ Int)

An example that fails:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [a] b => b -> Bool
    f x = m x == []

Which, because solving `Eq [a]` demands `Eq a` which we cannot solve, produces:

    f :: forall a b. C [a] b => b -> Bool
    f = \ (@ a) (@ b) ($dC :: C [a] b) (eta :: b) ->
        ==
          @ [a]
          (A.$p1C @ [a] @ b $dC)
          (m @ [a] @ b $dC eta)
          (GHC.Types.[] @ a)

Note [Shortcut solving: type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (#13943)
  class Take (n :: Nat) where ...
  instance {-# OVERLAPPING #-}                    Take 0 where ..
  instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where ..

And we have [W] Take 3.  That only matches one instance so we get
[W] Take (3-1).  Really we should now flatten to reduce the (3-1) to 2, and
so on -- but that is reproducing yet more of the solver.  Sigh.  For now,
we just give up (remember all this is just an optimisation).

But we must not just naively try to lookup (Take (3-1)) in the
InstEnv, or it'll (wrongly) appear not to match (Take 0) and get a
unique match on the (Take n) instance.  That leads immediately to an
infinite loop.  Hence the check that 'preds' have no type families
(isTyFamFree).

Note [Shortcut solving: incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This optimization relies on coherence of dictionaries to be correct. When we
cannot assume coherence because of IncoherentInstances then this optimization
can change the behavior of the user's code.

The following four modules produce a program whose output would change depending
on whether we apply this optimization when IncoherentInstances is in effect:

#########
    {-# LANGUAGE MultiParamTypeClasses #-}
    module A where

    class A a where
      int :: a -> Int

    class A a => C a b where
      m :: b -> a -> a

#########
    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
    module B where

    import A

    instance A a where
      int _ = 1

    instance C a [b] where
      m _ = id

#########
    {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
    {-# LANGUAGE IncoherentInstances #-}
    module C where

    import A

    instance A Int where
      int _ = 2

    instance C Int [Int] where
      m _ = id

    intC :: C Int a => a -> Int -> Int
    intC _ x = int x

#########
    module Main where

    import A
    import B
    import C

    main :: IO ()
    main = print (intC [] (0::Int))

The output of `main` if we avoid the optimization under the effect of
IncoherentInstances is `1`. If we were to do the optimization, the output of
`main` would be `2`.

Note [Shortcut try_solve_from_instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The workhorse of the short-cut solver is
    try_solve_from_instance :: (EvBindMap, DictMap CtEvidence)
                            -> CtEvidence       -- Solve this
                            -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
Note that:

* The CtEvidence is the goal to be solved

* The MaybeT manages early failure if we find a subgoal that
  cannot be solved from instances.

* The (EvBindMap, DictMap CtEvidence) is an accumulating purely-functional
  state that allows try_solve_from_instance to augmennt the evidence
  bindings and inert_solved_dicts as it goes.

  If it succeeds, we commit all these bindings and solved dicts to the
  main TcS InertSet.  If not, we abandon it all entirely.

Passing along the solved_dicts important for two reasons:

* We need to be able to handle recursive super classes. The
  solved_dicts state  ensures that we remember what we have already
  tried to solve to avoid looping.

* As #15164 showed, it can be important to exploit sharing between
  goals. E.g. To solve G we may need G1 and G2. To solve G1 we may need H;
  and to solve G2 we may need H. If we don't spot this sharing we may
  solve H twice; and if this pattern repeats we may get exponentially bad
  behaviour.
-}

interactDict :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactDict inerts workItem@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  | Just ct_i <- lookupInertDict inerts (ctEvLoc ev_w) cls tys
  , let ev_i = ctEvidence ct_i
  = -- There is a matching dictionary in the inert set
    do { -- First to try to solve it /completely/ from top level instances
         -- See Note [Shortcut solving]
         dflags <- getDynFlags
       ; short_cut_worked <- shortCutSolver dflags ev_w ev_i
       ; if short_cut_worked
         then stopWith ev_w "interactDict/solved from instance"
         else

    do { -- Ths short-cut solver didn't fire, so we
         -- solve ev_w from the matching inert ev_i we found
         what_next <- solveOneFromTheOther ev_i ev_w
       ; traceTcS "lookupInertDict" (ppr what_next)
       ; case what_next of
           KeepBoth  -> continueWith workItem
           KeepInert -> do { setEvBindIfWanted ev_w (ctEvTerm ev_i)
                           ; return $ Stop ev_w (text "Dict equal" <+> parens (ppr what_next)) }
           KeepWork  -> do { setEvBindIfWanted ev_i (ctEvTerm ev_w)
                           ; updInertDicts $ \ ds -> delDict ds cls tys
                           ; continueWith workItem } } }

  | cls `hasKey` ipClassKey
  , isGiven ev_w
  = interactGivenIP inerts workItem

  | otherwise
  = do { addFunDepWork inerts ev_w cls
       ; continueWith workItem  }

interactDict _ wi = pprPanic "interactDict" (ppr wi)

-- See Note [Shortcut solving]
shortCutSolver :: DynFlags
               -> CtEvidence -- Work item
               -> CtEvidence -- Inert we want to try to replace
               -> TcS Bool   -- True <=> success
shortCutSolver dflags ev_w ev_i
  | isWanted ev_w
 && isGiven ev_i
 -- We are about to solve a [W] constraint from a [G] constraint. We take
 -- a moment to see if we can get a better solution using an instance.
 -- Note that we only do this for the sake of performance. Exactly the same
 -- programs should typecheck regardless of whether we take this step or
 -- not. See Note [Shortcut solving]

 && not (isIPLikePred (ctEvPred ev_w))   -- Not for implicit parameters (#18627)

 && not (xopt LangExt.IncoherentInstances dflags)
 -- If IncoherentInstances is on then we cannot rely on coherence of proofs
 -- in order to justify this optimization: The proof provided by the
 -- [G] constraint's superclass may be different from the top-level proof.
 -- See Note [Shortcut solving: incoherence]

 && gopt Opt_SolveConstantDicts dflags
 -- Enabled by the -fsolve-constant-dicts flag

  = do { ev_binds_var <- getTcEvBindsVar
       ; ev_binds <- ASSERT2( not (isCoEvBindsVar ev_binds_var ), ppr ev_w )
                     getTcEvBindsMap ev_binds_var
       ; solved_dicts <- getSolvedDicts

       ; mb_stuff <- runMaybeT $ try_solve_from_instance
                                   (ev_binds, solved_dicts) ev_w

       ; case mb_stuff of
           Nothing -> return False
           Just (ev_binds', solved_dicts')
              -> do { setTcEvBindsMap ev_binds_var ev_binds'
                    ; setSolvedDicts solved_dicts'
                    ; return True } }

  | otherwise
  = return False
  where
    -- This `CtLoc` is used only to check the well-staged condition of any
    -- candidate DFun. Our subgoals all have the same stage as our root
    -- [W] constraint so it is safe to use this while solving them.
    loc_w = ctEvLoc ev_w

    try_solve_from_instance   -- See Note [Shortcut try_solve_from_instance]
      :: (EvBindMap, DictMap CtEvidence) -> CtEvidence
      -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
    try_solve_from_instance (ev_binds, solved_dicts) ev
      | let pred = ctEvPred ev
            loc  = ctEvLoc  ev
      , ClassPred cls tys <- classifyPredType pred
      = do { inst_res <- lift $ matchGlobalInst dflags True cls tys
           ; case inst_res of
               OneInst { cir_new_theta = preds
                       , cir_mk_ev     = mk_ev
                       , cir_what      = what }
                 | safeOverlap what
                 , all isTyFamFree preds  -- Note [Shortcut solving: type families]
                 -> do { let solved_dicts' = addDict solved_dicts cls tys ev
                             -- solved_dicts': it is important that we add our goal
                             -- to the cache before we solve! Otherwise we may end
                             -- up in a loop while solving recursive dictionaries.

                       ; lift $ traceTcS "shortCutSolver: found instance" (ppr preds)
                       ; loc' <- lift $ checkInstanceOK loc what pred

                       ; evc_vs <- mapM (new_wanted_cached loc' solved_dicts') preds
                                  -- Emit work for subgoals but use our local cache
                                  -- so we can solve recursive dictionaries.

                       ; let ev_tm     = mk_ev (map getEvExpr evc_vs)
                             ev_binds' = extendEvBinds ev_binds $
                                         mkWantedEvBind (ctEvEvId ev) ev_tm

                       ; foldlM try_solve_from_instance
                                (ev_binds', solved_dicts')
                                (freshGoals evc_vs) }

               _ -> mzero }
      | otherwise = mzero


    -- Use a local cache of solved dicts while emitting EvVars for new work
    -- We bail out of the entire computation if we need to emit an EvVar for
    -- a subgoal that isn't a ClassPred.
    new_wanted_cached :: CtLoc -> DictMap CtEvidence -> TcPredType -> MaybeT TcS MaybeNew
    new_wanted_cached loc cache pty
      | ClassPred cls tys <- classifyPredType pty
      = lift $ case findDict cache loc_w cls tys of
          Just ctev -> return $ Cached (ctEvExpr ctev)
          Nothing   -> Fresh <$> newWantedNC loc pty
      | otherwise = mzero

addFunDepWork :: InertCans -> CtEvidence -> Class -> TcS ()
-- Add derived constraints from type-class functional dependencies.
addFunDepWork inerts work_ev cls
  | isImprovable work_ev
  = mapBagM_ add_fds (findDictsByClass (inert_dicts inerts) cls)
               -- No need to check flavour; fundeps work between
               -- any pair of constraints, regardless of flavour
               -- Importantly we don't throw workitem back in the
               -- worklist because this can cause loops (see #5236)
  | otherwise
  = return ()
  where
    work_pred = ctEvPred work_ev
    work_loc  = ctEvLoc work_ev

    add_fds inert_ct
      | isImprovable inert_ev
      = do { traceTcS "addFunDepWork" (vcat
                [ ppr work_ev
                , pprCtLoc work_loc, ppr (isGivenLoc work_loc)
                , pprCtLoc inert_loc, ppr (isGivenLoc inert_loc)
                , pprCtLoc derived_loc, ppr (isGivenLoc derived_loc) ]) ;

        emitFunDepDeriveds $
        improveFromAnother derived_loc inert_pred work_pred
               -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
               -- NB: We do create FDs for given to report insoluble equations that arise
               -- from pairs of Givens, and also because of floating when we approximate
               -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
        }
      | otherwise
      = return ()
      where
        inert_ev   = ctEvidence inert_ct
        inert_pred = ctEvPred inert_ev
        inert_loc  = ctEvLoc inert_ev
        derived_loc = work_loc { ctl_depth  = ctl_depth work_loc `maxSubGoalDepth`
                                              ctl_depth inert_loc
                               , ctl_origin = FunDepOrigin1 work_pred
                                                            (ctLocOrigin work_loc)
                                                            (ctLocSpan work_loc)
                                                            inert_pred
                                                            (ctLocOrigin inert_loc)
                                                            (ctLocSpan inert_loc) }

{-
**********************************************************************
*                                                                    *
                   Implicit parameters
*                                                                    *
**********************************************************************
-}

interactGivenIP :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- Work item is Given (?x:ty)
-- See Note [Shadowing of Implicit Parameters]
interactGivenIP inerts workItem@(CDictCan { cc_ev = ev, cc_class = cls
                                          , cc_tyargs = tys@(ip_str:_) })
  = do { updInertCans $ \cans -> cans { inert_dicts = addDict filtered_dicts cls tys workItem }
       ; stopWith ev "Given IP" }
  where
    dicts           = inert_dicts inerts
    ip_dicts        = findDictsByClass dicts cls
    other_ip_dicts  = filterBag (not . is_this_ip) ip_dicts
    filtered_dicts  = addDictsByClass dicts cls other_ip_dicts

    -- Pick out any Given constraints for the same implicit parameter
    is_this_ip (CDictCan { cc_ev = ev, cc_tyargs = ip_str':_ })
       = isGiven ev && ip_str `tcEqType` ip_str'
    is_this_ip _ = False

interactGivenIP _ wi = pprPanic "interactGivenIP" (ppr wi)

{- Note [Shadowing of Implicit Parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char

Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in
two incompatible ways:  either by using the parameter from the
signature, or by using the local definition.  Our intention is
that the local definition should "shadow" the parameter of the
signature, and we implement this as follows: when we add a new
*given* implicit parameter to the inert set, it replaces any existing
givens for the same implicit parameter.

Similarly, consider
   f :: (?x::a) => Bool -> a

   g v = let ?x::Int = 3
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.

See ticket #17104 for a rather tricky example of this overriding
behaviour.

All this works for the normal cases but it has an odd side effect in
some pathological programs like this:
-- This is accepted, the second parameter shadows
f1 :: (?x :: Int, ?x :: Char) => Char
f1 = ?x

-- This is rejected, the second parameter shadows
f2 :: (?x :: Int, ?x :: Char) => Int
f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wanted constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constraints for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.


**********************************************************************
*                                                                    *
                   interactFunEq
*                                                                    *
**********************************************************************
-}

interactFunEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- Try interacting the work item with the inert set
interactFunEq inerts work_item@(CFunEqCan { cc_ev = ev, cc_fun = tc
                                          , cc_tyargs = args, cc_fsk = fsk })
  | Just inert_ct@(CFunEqCan { cc_ev = ev_i
                             , cc_fsk = fsk_i })
         <- findFunEq (inert_funeqs inerts) tc args
  , pr@(swap_flag, upgrade_flag) <- ev_i `funEqCanDischarge` ev
  = do { traceTcS "reactFunEq (rewrite inert item):" $
         vcat [ text "work_item =" <+> ppr work_item
              , text "inertItem=" <+> ppr ev_i
              , text "(swap_flag, upgrade)" <+> ppr pr ]
       ; if isSwapped swap_flag
         then do {   -- Rewrite inert using work-item
                   let work_item' | upgrade_flag = upgradeWanted work_item
                                  | otherwise    = work_item
                 ; updInertFunEqs $ \ feqs -> insertFunEq feqs tc args work_item'
                      -- Do the updInertFunEqs before the reactFunEq, so that
                      -- we don't kick out the inertItem as well as consuming it!
                 ; reactFunEq ev fsk ev_i fsk_i
                 ; stopWith ev "Work item rewrites inert" }
         else do {   -- Rewrite work-item using inert
                 ; when upgrade_flag $
                   updInertFunEqs $ \ feqs -> insertFunEq feqs tc args
                                                 (upgradeWanted inert_ct)
                 ; reactFunEq ev_i fsk_i ev fsk
                 ; stopWith ev "Inert rewrites work item" } }

  | otherwise   -- Try improvement
  = do { improveLocalFunEqs ev inerts tc args fsk
       ; continueWith work_item }

interactFunEq _ work_item = pprPanic "interactFunEq" (ppr work_item)

upgradeWanted :: Ct -> Ct
-- We are combining a [W] F tys ~ fmv1 and [D] F tys ~ fmv2
-- so upgrade the [W] to [WD] before putting it in the inert set
upgradeWanted ct = ct { cc_ev = upgrade_ev (cc_ev ct) }
  where
    upgrade_ev ev = ASSERT2( isWanted ev, ppr ct )
                    ev { ctev_nosh = WDeriv }

improveLocalFunEqs :: CtEvidence -> InertCans -> TyCon -> [TcType] -> TcTyVar
                   -> TcS ()
-- Generate derived improvement equalities, by comparing
-- the current work item with inert CFunEqs
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [D] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
improveLocalFunEqs work_ev inerts fam_tc args fsk
  | isGiven work_ev -- See Note [No FunEq improvement for Givens]
    || not (isImprovable work_ev)
  = return ()

  | otherwise
  = do { eqns <- improvement_eqns
       ; if not (null eqns)
         then do { traceTcS "interactFunEq improvements: " $
                   vcat [ text "Eqns:" <+> ppr eqns
                        , text "Candidates:" <+> ppr funeqs_for_tc
                        , text "Inert eqs:" <+> ppr (inert_eqs inerts) ]
                 ; emitFunDepDeriveds eqns }
         else return () }

  where
    funeqs        = inert_funeqs inerts
    funeqs_for_tc = findFunEqsByTyCon funeqs fam_tc
    work_loc      = ctEvLoc work_ev
    work_pred     = ctEvPred work_ev
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: TcS [FunDepEqn CtLoc]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
        do { rhs <- rewriteTyVar fsk
           ; concatMapM (do_one_built_in ops rhs) funeqs_for_tc }

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        do { rhs <- rewriteTyVar fsk
           ; concatMapM (do_one_injective injective_args rhs) funeqs_for_tc }

      | otherwise
      = return []

    --------------------
    do_one_built_in ops rhs (CFunEqCan { cc_tyargs = iargs, cc_fsk = ifsk, cc_ev = inert_ev })
      = do { inert_rhs <- rewriteTyVar ifsk
           ; return $ mk_fd_eqns inert_ev (sfInteractInert ops args rhs iargs inert_rhs) }

    do_one_built_in _ _ _ = pprPanic "interactFunEq 1" (ppr fam_tc)

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args rhs (CFunEqCan { cc_tyargs = inert_args
                                             , cc_fsk = ifsk, cc_ev = inert_ev })
      | isImprovable inert_ev
      = do { inert_rhs <- rewriteTyVar ifsk
           ; return $ if rhs `tcEqType` inert_rhs
                      then mk_fd_eqns inert_ev $
                             [ Pair arg iarg
                             | (arg, iarg, True) <- zip3 args inert_args inj_args ]
                      else [] }
      | otherwise
      = return []

    do_one_injective _ _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)

    --------------------
    mk_fd_eqns :: CtEvidence -> [TypeEqn] -> [FunDepEqn CtLoc]
    mk_fd_eqns inert_ev eqns
      | null eqns  = []
      | otherwise  = [ FDEqn { fd_qtvs = [], fd_eqs = eqns
                             , fd_pred1 = work_pred
                             , fd_pred2 = ctEvPred inert_ev
                             , fd_loc   = loc } ]
      where
        inert_loc = ctEvLoc inert_ev
        loc = inert_loc { ctl_depth = ctl_depth inert_loc `maxSubGoalDepth`
                                      ctl_depth work_loc }

-------------
reactFunEq :: CtEvidence -> TcTyVar    -- From this  :: F args1 ~ fsk1
           -> CtEvidence -> TcTyVar    -- Solve this :: F args2 ~ fsk2
           -> TcS ()
reactFunEq from_this fsk1 solve_this fsk2
  = do { traceTcS "reactFunEq"
            (vcat [ppr from_this, ppr fsk1, ppr solve_this, ppr fsk2])
       ; dischargeFunEq solve_this fsk2 (ctEvCoercion from_this) (mkTyVarTy fsk1)
       ; traceTcS "reactFunEq done" (ppr from_this $$ ppr fsk1 $$
                                     ppr solve_this $$ ppr fsk2) }

{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have two CFunEqCan constraints for F with the same RHS
   F s1 t1 ~ rhs
   F s2 t2 ~ rhs
then we can use the injectivity to get a new Derived constraint on
the injective argument
  [D] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and inferring values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are simply *Derived* constraints; they have no evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

See also Note [Injective type families] in GHC.Core.TyCon


Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.


Note [Efficient Orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are interacting two FunEqCans with the same LHS:
          (inert)  ci :: (F ty ~ xi_i)
          (work)   cw :: (F ty ~ xi_w)
We prefer to keep the inert (else we pass the work item on down
the pipeline, which is a bit silly).  If we keep the inert, we
will (a) discharge 'cw'
     (b) produce a new equality work-item (xi_w ~ xi_i)
Notice the orientation (xi_w ~ xi_i) NOT (xi_i ~ xi_w):
    new_work :: xi_w ~ xi_i
    cw := ci ; sym new_work
Why?  Consider the simplest case when xi1 is a type variable.  If
we generate xi1~xi2, processing that constraint will kick out 'ci'.
If we generate xi2~xi1, there is less chance of that happening.
Of course it can and should still happen if xi1=a, xi1=Int, say.
But we want to avoid it happening needlessly.

Similarly, if we *can't* keep the inert item (because inert is Wanted,
and work is Given, say), we prefer to orient the new equality (xi_i ~
xi_w).

Note [Carefully solve the right CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ---- OLD COMMENT, NOW NOT NEEDED
   ---- because we now allow multiple
   ---- wanted FunEqs with the same head
Consider the constraints
  c1 :: F Int ~ a      -- Arising from an application line 5
  c2 :: F Int ~ Bool   -- Arising from an application line 10
Suppose that 'a' is a unification variable, arising only from
flattening.  So there is no error on line 5; it's just a flattening
variable.  But there is (or might be) an error on line 10.

Two ways to combine them, leaving either (Plan A)
  c1 :: F Int ~ a      -- Arising from an application line 5
  c3 :: a ~ Bool       -- Arising from an application line 10
or (Plan B)
  c2 :: F Int ~ Bool   -- Arising from an application line 10
  c4 :: a ~ Bool       -- Arising from an application line 5

Plan A will unify c3, leaving c1 :: F Int ~ Bool as an error
on the *totally innocent* line 5.  An example is test SimpleFail16
where the expected/actual message comes out backwards if we use
the wrong plan.

The second is the right thing to do.  Hence the isMetaTyVarTy
test when solving pairwise CFunEqCan.


**********************************************************************
*                                                                    *
                   interactTyVarEq
*                                                                    *
**********************************************************************
-}

inertsCanDischarge :: InertCans -> TcTyVar -> TcType -> CtFlavourRole
                   -> Maybe ( CtEvidence  -- The evidence for the inert
                            , SwapFlag    -- Whether we need mkSymCo
                            , Bool)       -- True <=> keep a [D] version
                                          --          of the [WD] constraint
inertsCanDischarge inerts tv rhs fr
  | (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                    , cc_eq_rel = eq_rel }
                             <- findTyEqs inerts tv
                         , (ctEvFlavour ev_i, eq_rel) `eqCanDischargeFR` fr
                         , rhs_i `tcEqType` rhs ]
  =  -- Inert:     a ~ ty
     -- Work item: a ~ ty
    Just (ev_i, NotSwapped, keep_deriv ev_i)

  | Just tv_rhs <- getTyVar_maybe rhs
  , (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                    , cc_eq_rel = eq_rel }
                             <- findTyEqs inerts tv_rhs
                         , (ctEvFlavour ev_i, eq_rel) `eqCanDischargeFR` fr
                         , rhs_i `tcEqType` mkTyVarTy tv ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
     Just (ev_i, IsSwapped, keep_deriv ev_i)

  | otherwise
  = Nothing

  where
    keep_deriv ev_i
      | Wanted WOnly  <- ctEvFlavour ev_i  -- inert is [W]
      , (Wanted WDeriv, _) <- fr           -- work item is [WD]
      = True   -- Keep a derived version of the work item
      | otherwise
      = False  -- Work item is fully discharged

interactTyVarEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- CTyEqCans are always consumed, so always returns Stop
interactTyVarEq inerts workItem@(CTyEqCan { cc_tyvar = tv
                                          , cc_rhs = rhs
                                          , cc_ev = ev
                                          , cc_eq_rel = eq_rel })
  | Just (ev_i, swapped, keep_deriv)
       <- inertsCanDischarge inerts tv rhs (ctEvFlavour ev, eq_rel)
  = do { setEvBindIfWanted ev $
         evCoercion (maybeSym swapped $
                     tcDowngradeRole (eqRelRole eq_rel)
                                     (ctEvRole ev_i)
                                     (ctEvCoercion ev_i))

       ; let deriv_ev = CtDerived { ctev_pred = ctEvPred ev
                                  , ctev_loc  = ctEvLoc  ev }
       ; when keep_deriv $
         emitWork [workItem { cc_ev = deriv_ev }]
         -- As a Derived it might not be fully rewritten,
         -- so we emit it as new work

       ; stopWith ev "Solved from inert" }

  | ReprEq <- eq_rel   -- See Note [Do not unify representational equalities]
  = do { traceTcS "Not unifying representational equality" (ppr workItem)
       ; continueWith workItem }

  | isGiven ev         -- See Note [Touchables and givens]
  = continueWith workItem

  | otherwise
  = do { tclvl <- getTcLevel
       ; if canSolveByUnification tclvl tv rhs
         then do { solveByUnification ev tv rhs
                 ; n_kicked <- kickOutAfterUnification tv
                 ; return (Stop ev (text "Solved by unification" <+> pprKicked n_kicked)) }

         else continueWith workItem }

interactTyVarEq _ wi = pprPanic "interactTyVarEq" (ppr wi)

solveByUnification :: CtEvidence -> TcTyVar -> Xi -> TcS ()
-- Solve with the identity coercion
-- Precondition: kind(xi) equals kind(tv)
-- Precondition: CtEvidence is Wanted or Derived
-- Precondition: CtEvidence is nominal
-- Returns: workItem where
--        workItem = the new Given constraint
--
-- NB: No need for an occurs check here, because solveByUnification always
--     arises from a CTyEqCan, a *canonical* constraint.  Its invariant (TyEq:OC)
--     says that in (a ~ xi), the type variable a does not appear in xi.
--     See GHC.Tc.Types.Constraint.Ct invariants.
--
-- Post: tv is unified (by side effect) with xi;
--       we often write tv := xi
solveByUnification wd tv xi
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "Sneaky unification:" $
                       vcat [text "Unifies:" <+> ppr tv <+> text ":=" <+> ppr xi,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (tcTypeKind tv_ty),
                             text "Right Kind is:" <+> ppr (tcTypeKind xi) ]

       ; unifyTyVar tv xi
       ; setEvBindIfWanted wd (evCoercion (mkTcNomReflCo xi)) }

{- Note [Avoid double unifications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not:
  Original wanted:  (a ~ alpha),  (alpha ~ Int)
We spontaneously solve the first wanted, without changing the order!
      given : a ~ alpha      [having unified alpha := a]
Now the second wanted comes along, but he cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int]

We avoid this problem by orienting the resulting given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

See also Note [No touchables as FunEq RHS] in GHC.Tc.Solver.Monad; avoiding
double unifications is the main reason we disallow touchable
unification variables as RHS of type family equations: F xis ~ alpha.

Note [Do not unify representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   [W] alpha ~R# b
where alpha is touchable. Should we unify alpha := b?

Certainly not!  Unifying forces alpha and be to be the same; but they
only need to be representationally equal types.

For example, we might have another constraint [W] alpha ~# N b
where
  newtype N b = MkN b
and we want to get alpha := N b.

See also #15144, which was caused by unifying a representational
equality (in the unflattener).


************************************************************************
*                                                                      *
*          Functional dependencies, instantiation of equations
*                                                                      *
************************************************************************

When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
           situations (when a Given overlaps with an instance) that
           may produce new insoluble goals: see #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of GHC.Tc.Instance.FunDeps (nicer
now!).

Note [FunDep and implicit parameter reactions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
parameters, is that we simply produce new Derived equalities.  So for example

        class D a b | a -> b where ...
    Inert:
        d1 :g D Int Bool
    WorkItem:
        d2 :w D Int alpha

    We generate the extra work item
        cv :d alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
        d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary
using those extra equalities, but that requires those equalities to carry
evidence and derived do not carry evidence.

If that were the case with the same inert set and work item we might dischard
d2 directly:

        cv :w alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b
And:
     [Given]  d1 : C T Int Char
     [Wanted] d2 : C T beta Int
In this case, it's *not even possible* to solve the wanted immediately.
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse:
     [Given] d1 : C T Int beta
     [Wanted] d2: C T beta Int
Then it is solvable, but its very hard to detect this on the spot.

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

Note [Weird fundeps]
~~~~~~~~~~~~~~~~~~~~
Consider   class Het a b | a -> b where
              het :: m (f c) -> a -> m b

           class GHet (a :: * -> *) (b :: * -> *) | a -> b
           instance            GHet (K a) (K [a])
           instance Het a b => GHet (K a) (K b)

The two instances don't actually conflict on their fundeps,
although it's pretty strange.  So they are both accepted. Now
try   [W] GHet (K Int) (K Bool)
This triggers fundeps from both instance decls;
      [D] K Bool ~ K [a]
      [D] K Bool ~ K beta
And there's a risk of complaining about Bool ~ [a].  But in fact
the Wanted matches the second instance, so we never get as far
as the fundeps.

#7875 is a case in point.
-}

emitFunDepDeriveds :: [FunDepEqn CtLoc] -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
emitFunDepDeriveds fd_eqns
  = mapM_ do_one_FDEqn fd_eqns
  where
    do_one_FDEqn (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = loc })
     | null tvs  -- Common shortcut
     = do { traceTcS "emitFunDepDeriveds 1" (ppr (ctl_depth loc) $$ ppr eqs $$ ppr (isGivenLoc loc))
          ; mapM_ (unifyDerived loc Nominal) eqs }
     | otherwise
     = do { traceTcS "emitFunDepDeriveds 2" (ppr (ctl_depth loc) $$ ppr tvs $$ ppr eqs)
          ; subst <- instFlexi tvs  -- Takes account of kind substitution
          ; mapM_ (do_one_eq loc subst) eqs }

    do_one_eq loc subst (Pair ty1 ty2)
       = unifyDerived loc Nominal $
         Pair (Type.substTyUnchecked subst ty1) (Type.substTyUnchecked subst ty2)

{-
**********************************************************************
*                                                                    *
                       The top-reaction Stage
*                                                                    *
**********************************************************************
-}

topReactionsStage :: WorkItem -> TcS (StopOrContinue Ct)
-- The work item does not react with the inert set,
-- so try interaction with top-level instances. Note:
topReactionsStage work_item
  = do { traceTcS "doTopReact" (ppr work_item)
       ; case work_item of
           CDictCan {}  -> do { inerts <- getTcSInerts
                              ; doTopReactDict inerts work_item }
           CFunEqCan {} -> doTopReactFunEq work_item
           CIrredCan {} -> doTopReactOther work_item
           CTyEqCan {}  -> doTopReactOther work_item
           _  -> -- Any other work item does not react with any top-level equations
                 continueWith work_item  }


--------------------
doTopReactOther :: Ct -> TcS (StopOrContinue Ct)
-- Try local quantified constraints for
--     CTyEqCan  e.g.  (a ~# ty)
-- and CIrredCan e.g.  (c a)
--
-- Why equalities? See GHC.Tc.Solver.Canonical
-- Note [Equality superclasses in quantified constraints]
doTopReactOther work_item
  | isGiven ev
  = continueWith work_item

  | EqPred eq_rel t1 t2 <- classifyPredType pred
  = doTopReactEqPred work_item eq_rel t1 t2

  | otherwise
  = do { res <- matchLocalInst pred loc
       ; case res of
           OneInst {} -> chooseInstance work_item res
           _          -> continueWith work_item }

  where
    ev   = ctEvidence work_item
    loc  = ctEvLoc ev
    pred = ctEvPred ev

doTopReactEqPred :: Ct -> EqRel -> TcType -> TcType -> TcS (StopOrContinue Ct)
doTopReactEqPred work_item eq_rel t1 t2
  -- See Note [Looking up primitive equalities in quantified constraints]
  | Just (cls, tys) <- boxEqPred eq_rel t1 t2
  = do { res <- matchLocalInst (mkClassPred cls tys) loc
       ; case res of
           OneInst { cir_mk_ev = mk_ev }
             -> chooseInstance work_item
                    (res { cir_mk_ev = mk_eq_ev cls tys mk_ev })
           _ -> continueWith work_item }

  | otherwise
  = continueWith work_item
  where
    ev   = ctEvidence work_item
    loc = ctEvLoc ev

    mk_eq_ev cls tys mk_ev evs
      = case (mk_ev evs) of
          EvExpr e -> EvExpr (Var sc_id `mkTyApps` tys `App` e)
          ev       -> pprPanic "mk_eq_ev" (ppr ev)
      where
        [sc_id] = classSCSelIds cls

{- Note [Looking up primitive equalities in quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For equalities (a ~# b) look up (a ~ b), and then do a superclass
selection. This avoids having to support quantified constraints whose
kind is not Constraint, such as (forall a. F a ~# b)

See
 * Note [Evidence for quantified constraints] in GHC.Core.Predicate
 * Note [Equality superclasses in quantified constraints]
   in GHC.Tc.Solver.Canonical

Note [Flatten when discharging CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have the following scenario (#16512):

type family LV (as :: [Type]) (b :: Type) = (r :: Type) | r -> as b where
  LV (a ': as) b = a -> LV as b

[WD] w1 :: LV as0 (a -> b) ~ fmv1 (CFunEqCan)
[WD] w2 :: fmv1 ~ (a -> fmv2) (CTyEqCan)
[WD] w3 :: LV as0 b ~ fmv2 (CFunEqCan)

We start with w1. Because LV is injective, we wish to see if the RHS of the
equation matches the RHS of the CFunEqCan. The RHS of a CFunEqCan is always an
fmv, so we "look through" to get (a -> fmv2). Then we run tcUnifyTyWithTFs.
That performs the match, but it allows a type family application (such as the
LV in the RHS of the equation) to match with anything. (See "Injective type
families" by Stolarek et al., HS'15, Fig. 2) The matching succeeds, which
means we can improve as0 (and b, but that's not interesting here). However,
because the RHS of w1 can't see through fmv2 (we have no way of looking up a
LHS of a CFunEqCan from its RHS, and this use case isn't compelling enough),
we invent a new unification variable here. We thus get (as0 := a : as1).
Rewriting:

[WD] w1 :: LV (a : as1) (a -> b) ~ fmv1
[WD] w2 :: fmv1 ~ (a -> fmv2)
[WD] w3 :: LV (a : as1) b ~ fmv2

We can now reduce both CFunEqCans, using the equation for LV. We get

[WD] w2 :: (a -> LV as1 (a -> b)) ~ (a -> a -> LV as1 b)

Now we decompose (and flatten) to

[WD] w4 :: LV as1 (a -> b) ~ fmv3
[WD] w5 :: fmv3 ~ (a -> fmv1)
[WD] w6 :: LV as1 b ~ fmv4

which is exactly where we started. These goals really are insoluble, but
we would prefer not to loop. We thus need to find a way to bump the reduction
depth, so that we can detect the loop and abort.

The key observation is that we are performing a reduction. We thus wish
to bump the level when discharging a CFunEqCan. Where does this bumped
level go, though? It can't just go on the reduct, as that's a type. Instead,
it must go on any CFunEqCans produced after flattening. We thus flatten
when discharging, making sure that the level is bumped in the new
fun-eqs. The flattening happens in reduce_top_fun_eq and the level
is bumped when setting up the FlatM monad in GHC.Tc.Solver.Flatten.runFlatten.
(This bumping will happen for call sites other than this one, but that
makes sense -- any constraints emitted by the flattener are offshoots
the work item and should have a higher level. We don't have any test
cases that require the bumping in this other cases, but it's convenient
and causes no harm to bump at every flatten.)

Test case: typecheck/should_fail/T16512a

-}

--------------------
doTopReactFunEq :: Ct -> TcS (StopOrContinue Ct)
doTopReactFunEq work_item@(CFunEqCan { cc_ev = old_ev, cc_fun = fam_tc
                                     , cc_tyargs = args, cc_fsk = fsk })

  | fsk `elemVarSet` tyCoVarsOfTypes args
  = no_reduction    -- See Note [FunEq occurs-check principle]

  | otherwise  -- Note [Reduction for Derived CFunEqCans]
  = do { match_res <- matchFam fam_tc args
                           -- Look up in top-level instances, or built-in axiom
                           -- See Note [MATCHING-SYNONYMS]
       ; case match_res of
           Nothing         -> no_reduction
           Just match_info -> reduce_top_fun_eq old_ev fsk match_info }
  where
    no_reduction
      = do { improveTopFunEqs old_ev fam_tc args fsk
           ; continueWith work_item }

doTopReactFunEq w = pprPanic "doTopReactFunEq" (ppr w)

reduce_top_fun_eq :: CtEvidence -> TcTyVar -> (TcCoercion, TcType)
                  -> TcS (StopOrContinue Ct)
-- We have found an applicable top-level axiom: use it to reduce
-- Precondition: fsk is not free in rhs_ty
-- ax_co :: F tys ~ rhs_ty, where F tys is the LHS of the old_ev
reduce_top_fun_eq old_ev fsk (ax_co, rhs_ty)
  | not (isDerived old_ev)  -- Precondition of shortCutReduction
  , Just (tc, tc_args) <- tcSplitTyConApp_maybe rhs_ty
  , isTypeFamilyTyCon tc
  , tc_args `lengthIs` tyConArity tc    -- Short-cut
  = -- RHS is another type-family application
    -- Try shortcut; see Note [Top-level reductions for type functions]
    do { shortCutReduction old_ev fsk ax_co tc tc_args
       ; stopWith old_ev "Fun/Top (shortcut)" }

  | otherwise
  = ASSERT2( not (fsk `elemVarSet` tyCoVarsOfType rhs_ty)
           , ppr old_ev $$ ppr rhs_ty )
           -- Guaranteed by Note [FunEq occurs-check principle]
    do { (rhs_xi, flatten_co) <- flatten FM_FlattenAll old_ev rhs_ty
             -- flatten_co :: rhs_xi ~ rhs_ty
             -- See Note [Flatten when discharging CFunEqCan]
       ; let total_co = ax_co `mkTcTransCo` mkTcSymCo flatten_co
       ; dischargeFunEq old_ev fsk total_co rhs_xi
       ; traceTcS "doTopReactFunEq" $
         vcat [ text "old_ev:" <+> ppr old_ev
              , nest 2 (text ":=") <+> ppr ax_co ]
       ; stopWith old_ev "Fun/Top" }

improveTopFunEqs :: CtEvidence -> TyCon -> [TcType] -> TcTyVar -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs ev fam_tc args fsk
  | isGiven ev            -- See Note [No FunEq improvement for Givens]
    || not (isImprovable ev)
  = return ()

  | otherwise
  = do { fam_envs <- getFamInstEnvs
       ; rhs <- rewriteTyVar fsk
       ; eqns <- improve_top_fun_eqs fam_envs fam_tc args rhs
       ; traceTcS "improveTopFunEqs" (vcat [ ppr fam_tc <+> ppr args <+> ppr rhs
                                          , ppr eqns ])
       ; mapM_ (unifyDerived loc Nominal) eqns }
  where
    loc = bumpCtLocDepth (ctEvLoc ev)
        -- ToDo: this location is wrong; it should be FunDepOrigin2
        -- See #14778

improve_top_fun_eqs :: FamInstEnvs
                    -> TyCon -> [TcType] -> TcType
                    -> TcS [TypeEqn]
improve_top_fun_eqs fam_envs fam_tc args rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return (sfInteractTop ops args rhs_ty)

  -- see Note [Type inference for type families with injectivity]
  | isOpenTypeFamilyTyCon fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  , let fam_insts = lookupFamInstEnvByTyCon fam_envs fam_tc
  = -- it is possible to have several compatible equations in an open type
    -- family but we only want to derive equalities from one such equation.
    do { let improvs = buildImprovementData fam_insts
                           fi_tvs fi_tys fi_rhs (const Nothing)

       ; traceTcS "improve_top_fun_eqs2" (ppr improvs)
       ; concatMapM (injImproveEqns injective_args) $
         take 1 improvs }

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  = concatMapM (injImproveEqns injective_args) $
    buildImprovementData (fromBranches (co_ax_branches ax))
                         cab_tvs cab_lhs cab_rhs Just

  | otherwise
  = return []

  where
      buildImprovementData
          :: [a]                     -- axioms for a TF (FamInst or CoAxBranch)
          -> (a -> [TyVar])          -- get bound tyvars of an axiom
          -> (a -> [Type])           -- get LHS of an axiom
          -> (a -> Type)             -- get RHS of an axiom
          -> (a -> Maybe CoAxBranch) -- Just => apartness check required
          -> [( [Type], TCvSubst, [TyVar], Maybe CoAxBranch )]
             -- Result:
             -- ( [arguments of a matching axiom]
             -- , RHS-unifying substitution
             -- , axiom variables without substitution
             -- , Maybe matching axiom [Nothing - open TF, Just - closed TF ] )
      buildImprovementData axioms axiomTVs axiomLHS axiomRHS wrap =
          [ (ax_args, subst, unsubstTvs, wrap axiom)
          | axiom <- axioms
          , let ax_args = axiomLHS axiom
                ax_rhs  = axiomRHS axiom
                ax_tvs  = axiomTVs axiom
          , Just subst <- [tcUnifyTyWithTFs False ax_rhs rhs_ty]
          , let notInSubst tv = not (tv `elemVarEnv` getTvSubstEnv subst)
                unsubstTvs    = filter (notInSubst <&&> isTyVar) ax_tvs ]
                   -- The order of unsubstTvs is important; it must be
                   -- in telescope order e.g. (k:*) (a:k)

      injImproveEqns :: [Bool]
                     -> ([Type], TCvSubst, [TyCoVar], Maybe CoAxBranch)
                     -> TcS [TypeEqn]
      injImproveEqns inj_args (ax_args, subst, unsubstTvs, cabr)
        = do { subst <- instFlexiX subst unsubstTvs
                  -- If the current substitution bind [k -> *], and
                  -- one of the un-substituted tyvars is (a::k), we'd better
                  -- be sure to apply the current substitution to a's kind.
                  -- Hence instFlexiX.   #13135 was an example.

             ; return [ Pair (substTyUnchecked subst ax_arg) arg
                        -- NB: the ax_arg part is on the left
                        -- see Note [Improvement orientation]
                      | case cabr of
                          Just cabr' -> apartnessCheck (substTys subst ax_args) cabr'
                          _          -> True
                      , (ax_arg, arg, True) <- zip3 ax_args args inj_args ] }


shortCutReduction :: CtEvidence -> TcTyVar -> TcCoercion
                  -> TyCon -> [TcType] -> TcS ()
-- See Note [Top-level reductions for type functions]
-- Previously, we flattened the tc_args here, but there's no need to do so.
-- And, if we did, this function would have all the complication of
-- GHC.Tc.Solver.Canonical.canCFunEqCan. See Note [canCFunEqCan]
shortCutReduction old_ev fsk ax_co fam_tc tc_args
  = ASSERT( ctEvEqRel old_ev == NomEq)
               -- ax_co :: F args ~ G tc_args
               -- old_ev :: F args ~ fsk
    do { new_ev <- case ctEvFlavour old_ev of
           Given -> newGivenEvVar deeper_loc
                         ( mkPrimEqPred (mkTyConApp fam_tc tc_args) (mkTyVarTy fsk)
                         , evCoercion (mkTcSymCo ax_co
                                       `mkTcTransCo` ctEvCoercion old_ev) )

           Wanted {} ->
             -- See TcCanonical Note [Equalities with incompatible kinds] about NoBlockSubst
             do { (new_ev, new_co) <- newWantedEq_SI NoBlockSubst WDeriv deeper_loc Nominal
                                        (mkTyConApp fam_tc tc_args) (mkTyVarTy fsk)
                ; setWantedEq (ctev_dest old_ev) $ ax_co `mkTcTransCo` new_co
                ; return new_ev }

           Derived -> pprPanic "shortCutReduction" (ppr old_ev)

       ; let new_ct = CFunEqCan { cc_ev = new_ev, cc_fun = fam_tc
                                , cc_tyargs = tc_args, cc_fsk = fsk }
       ; updWorkListTcS (extendWorkListFunEq new_ct) }
  where
    deeper_loc = bumpCtLocDepth (ctEvLoc old_ev)

{- Note [Top-level reductions for type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c.f. Note [The flattening story] in GHC.Tc.Solver.Flatten

Suppose we have a CFunEqCan  F tys ~ fmv/fsk, and a matching axiom.
Here is what we do, in four cases:

* Wanteds: general firing rule
    (work item) [W]        x : F tys ~ fmv
    instantiate axiom: ax_co : F tys ~ rhs

   Then:
      Discharge   fmv := rhs
      Discharge   x := ax_co ; sym x2
   This is *the* way that fmv's get unified; even though they are
   "untouchable".

   NB: Given Note [FunEq occurs-check principle], fmv does not appear
   in tys, and hence does not appear in the instantiated RHS.  So
   the unification can't make an infinite type.

* Wanteds: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [W] x : F tys ~ fmv
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fmv for (G rhs_tys).
  Instead (shortCutReduction):
      - Flatten rhs_tys (cos : rhs_tys ~ rhs_xis)
      - Add G rhs_xis ~ fmv to flat cache  (note: the same old fmv)
      - New canonical wanted   [W] x2 : G rhs_xis ~ fmv  (CFunEqCan)
      - Discharge x := ax_co ; G cos ; x2

* Givens: general firing rule
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ rhs

   Now add non-canonical given (since rhs is not flat)
      [G] (sym g ; ax_co) : fsk ~ rhs  (Non-canonical)

* Givens: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fsk for (G rhs_tys).
  Instead (shortCutReduction):
     - Flatten rhs_tys: flat_cos : tys ~ flat_tys
     - Add new Canonical given
          [G] (sym (G flat_cos) ; co ; g) : G flat_tys ~ fsk   (CFunEqCan)

Note [FunEq occurs-check principle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I have spent a lot of time finding a good way to deal with
CFunEqCan constraints like
    F (fuv, a) ~ fuv
where flatten-skolem occurs on the LHS.  Now in principle we
might may progress by doing a reduction, but in practice its
hard to find examples where it is useful, and easy to find examples
where we fall into an infinite reduction loop.  A rule that works
very well is this:

  *** FunEq occurs-check principle ***

      Do not reduce a CFunEqCan
          F tys ~ fsk
      if fsk appears free in tys
      Instead we treat it as stuck.

Examples:

* #5837 has [G] a ~ TF (a,Int), with an instance
    type instance TF (a,b) = (TF a, TF b)
  This readily loops when solving givens.  But with the FunEq occurs
  check principle, it rapidly gets stuck which is fine.

* #12444 is a good example, explained in comment:2.  We have
    type instance F (Succ x) = Succ (F x)
    [W] alpha ~ Succ (F alpha)
  If we allow the reduction to happen, we get an infinite loop

Note [Cached solved FunEqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `funEqCanDischarge` because both uses might (say) be Wanteds,
and we *still* want to save the re-computation.

Note [MATCHING-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~
When trying to match a dictionary (D tau) to a top-level instance, or a
type family equation (F taus_1 ~ tau_2) to a top-level family instance,
we do *not* need to expand type synonyms because the matcher will do that for us.

Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A very delicate point is the orientation of derived equalities
arising from injectivity improvement (#12522).  Suppose we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] TF (alpha, beta) ~ fuv
  [W] fuv ~ (Int, <some type>)

The injectivity will give rise to derived constraints

  [D] gamma1 ~ alpha
  [D] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [D] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we grough it back in, it'd look like
   [W] TF (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new derived
equality with the template on the left.  Delicate, but it works.

Note [No FunEq improvement for Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't do improvements (injectivity etc) for Givens. Why?

* It generates Derived constraints on skolems, which don't do us
  much good, except perhaps identify inaccessible branches.
  (They'd be perfectly valid though.)

* For type-nat stuff the derived constraints include type families;
  e.g.  (a < b), (b < c) ==> a < c If we generate a Derived for this,
  we'll generate a Derived/Wanted CFunEqCan; and, since the same
  InertCans (after solving Givens) are used for each iteration, that
  massively confused the unflattening step (GHC.Tc.Solver.Flatten.unflatten).

  In fact it led to some infinite loops:
     indexed-types/should_compile/T10806
     indexed-types/should_compile/T10507
     polykinds/T10742

Note [Reduction for Derived CFunEqCans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You may wonder if it's important to use top-level instances to
simplify [D] CFunEqCan's.  But it is.  Here's an example (T10226).

   type instance F    Int = Int
   type instance FInv Int = Int

Suppose we have to solve
    [WD] FInv (F alpha) ~ alpha
    [WD] F alpha ~ Int

  --> flatten
    [WD] F alpha ~ fuv0
    [WD] FInv fuv0 ~ fuv1  -- (A)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int        -- (B)

  --> Rewwrite (A) with (B), splitting it
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] FInv Int ~ fuv1    -- (C)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

  --> Reduce (C) with top-level instance
      **** This is the key step ***
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] fuv1 ~ Int        -- (D)
    [WD] fuv1 ~ alpha     -- (E)
    [WD] fuv0 ~ Int

  --> Rewrite (D) with (E)
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] alpha ~ Int       -- (F)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

  --> unify (F)  alpha := Int, and that solves it

Another example is indexed-types/should_compile/T10634
-}

{- *******************************************************************
*                                                                    *
         Top-level reaction for class constraints (CDictCan)
*                                                                    *
**********************************************************************-}

doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = ev, cc_class = cls
                                          , cc_tyargs = xis })
  | isGiven ev   -- Never use instances for Given constraints
  = do { try_fundep_improvement
       ; continueWith work_item }

  | Just solved_ev <- lookupSolvedDict inerts dict_loc cls xis   -- Cached
  = do { setEvBindIfWanted ev (ctEvTerm solved_ev)
       ; stopWith ev "Dict/Top (cached)" }

  | otherwise  -- Wanted or Derived, but not cached
   = do { dflags <- getDynFlags
        ; lkup_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_res of
               OneInst { cir_what = what }
                  -> do { insertSafeOverlapFailureTcS what work_item
                        ; addSolvedDict what ev cls xis
                        ; chooseInstance work_item lkup_res }
               _  ->  -- NoInstance or NotSure
                     do { when (isImprovable ev) $
                          try_fundep_improvement
                        ; continueWith work_item } }
   where
     dict_pred   = mkClassPred cls xis
     dict_loc    = ctEvLoc ev
     dict_origin = ctLocOrigin dict_loc

     -- We didn't solve it; so try functional dependencies with
     -- the instance environment, and return
     -- See also Note [Weird fundeps]
     try_fundep_improvement
        = do { traceTcS "try_fundeps" (ppr work_item)
             ; instEnvs <- getInstEnvs
             ; emitFunDepDeriveds $
               improveFromInstEnv instEnvs mk_ct_loc dict_pred }

     mk_ct_loc :: PredType   -- From instance decl
               -> SrcSpan    -- also from instance deol
               -> CtLoc
     mk_ct_loc inst_pred inst_loc
       = dict_loc { ctl_origin = FunDepOrigin2 dict_pred dict_origin
                                               inst_pred inst_loc }

doTopReactDict _ w = pprPanic "doTopReactDict" (ppr w)


chooseInstance :: Ct -> ClsInstResult -> TcS (StopOrContinue Ct)
chooseInstance work_item
               (OneInst { cir_new_theta = theta
                        , cir_what      = what
                        , cir_mk_ev     = mk_ev })
  = do { traceTcS "doTopReact/found instance for" $ ppr ev
       ; deeper_loc <- checkInstanceOK loc what pred
       ; if isDerived ev then finish_derived deeper_loc theta
                         else finish_wanted  deeper_loc theta mk_ev }
  where
     ev         = ctEvidence work_item
     pred       = ctEvPred ev
     loc        = ctEvLoc ev

     finish_wanted :: CtLoc -> [TcPredType]
                   -> ([EvExpr] -> EvTerm) -> TcS (StopOrContinue Ct)
      -- Precondition: evidence term matches the predicate workItem
     finish_wanted loc theta mk_ev
        = do { evb <- getTcEvBindsVar
             ; if isCoEvBindsVar evb
               then -- See Note [Instances in no-evidence implications]
                    continueWith work_item
               else
          do { evc_vars <- mapM (newWanted loc) theta
             ; setEvBindIfWanted ev (mk_ev (map getEvExpr evc_vars))
             ; emitWorkNC (freshGoals evc_vars)
             ; stopWith ev "Dict/Top (solved wanted)" } }

     finish_derived loc theta
       = -- Use type-class instances for Deriveds, in the hope
         -- of generating some improvements
         -- C.f. Example 3 of Note [The improvement story]
         -- It's easy because no evidence is involved
         do { emitNewDeriveds loc theta
            ; traceTcS "finish_derived" (ppr (ctl_depth loc))
            ; stopWith ev "Dict/Top (solved derived)" }

chooseInstance work_item lookup_res
  = pprPanic "chooseInstance" (ppr work_item $$ ppr lookup_res)

checkInstanceOK :: CtLoc -> InstanceWhat -> TcPredType -> TcS CtLoc
-- Check that it's OK to use this insstance:
--    (a) the use is well staged in the Template Haskell sense
--    (b) we have not recursed too deep
-- Returns the CtLoc to used for sub-goals
checkInstanceOK loc what pred
  = do { checkWellStagedDFun loc what pred
       ; checkReductionDepth deeper_loc pred
       ; return deeper_loc }
  where
     deeper_loc = zap_origin (bumpCtLocDepth loc)
     origin     = ctLocOrigin loc

     zap_origin loc  -- After applying an instance we can set ScOrigin to
                     -- infinity, so that prohibitedSuperClassSolve never fires
       | ScOrigin {} <- origin
       = setCtLocOrigin loc (ScOrigin infinity)
       | otherwise
       = loc

{- Note [Instances in no-evidence implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #15290 we had
  [G] forall p q. Coercible p q => Coercible (m p) (m q))
  [W] forall <no-ev> a. m (Int, IntStateT m a)
                          ~R#
                        m (Int, StateT Int m a)

The Given is an ordinary quantified constraint; the Wanted is an implication
equality that arises from
  [W] (forall a. t1) ~R# (forall a. t2)

But because the (t1 ~R# t2) is solved "inside a type" (under that forall a)
we can't generate any term evidence.  So we can't actually use that
lovely quantified constraint.  Alas!

This test arranges to ignore the instance-based solution under these
(rare) circumstances.   It's sad, but I  really don't see what else we can do.
-}


matchClassInst :: DynFlags -> InertSet
               -> Class -> [Type]
               -> CtLoc -> TcS ClsInstResult
matchClassInst dflags inerts clas tys loc
-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use any instance
-- whether top level, or local quantified constraints.
-- ee Note [Instance and Given overlap]
  | not (xopt LangExt.IncoherentInstances dflags)
  , not (naturallyCoherentClass clas)
  , let matchable_givens = matchableGivens loc pred inerts
  , not (isEmptyBag matchable_givens)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item=" <+> pprClassPred clas tys
                , text "Potential matching givens:" <+> ppr matchable_givens ]
       ; return NotSure }

  | otherwise
  = do { traceTcS "matchClassInst" $ text "pred =" <+> ppr pred <+> char '{'
       ; local_res <- matchLocalInst pred loc
       ; case local_res of
           OneInst {} ->  -- See Note [Local instances and incoherence]
                do { traceTcS "} matchClassInst local match" $ ppr local_res
                   ; return local_res }

           NotSure -> -- In the NotSure case for local instances
                      -- we don't want to try global instances
                do { traceTcS "} matchClassInst local not sure" empty
                   ; return local_res }

           NoInstance  -- No local instances, so try global ones
              -> do { global_res <- matchGlobalInst dflags False clas tys
                    ; traceTcS "} matchClassInst global result" $ ppr global_res
                    ; return global_res } }
  where
    pred = mkClassPred clas tys

-- | If a class is "naturally coherent", then we needn't worry at all, in any
-- way, about overlapping/incoherent instances. Just solve the thing!
-- See Note [Naturally coherent classes]
-- See also Note [The equality class story] in "GHC.Builtin.Types.Prim".
naturallyCoherentClass :: Class -> Bool
naturallyCoherentClass cls
  = isCTupleClass cls
    || cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey


{- Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

From 'g' we get the implication constraint:
            forall a. Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The partial solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones.  But not skolems like 'a' in
  the implication constraint above.

The end effect is that, much as we do for overlapping instances, we
delay choosing a class instance if there is a possibility of another
instance OR a given to match our constraint later on. This fixes
#4981 and #5002.

Other notes:

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* Flatten-skolems: we do not treat a flatten-skolem as unifiable
  for this purpose.
  E.g.   f :: Eq (F a) => [a] -> [a]
         f xs = ....(xs==xs).....
  Here we get [W] Eq [a], and we don't want to refrain from solving
  it because of the given (Eq (F a)) constraint!

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is #10195; another is #10177.

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see #6002 for a worked-out example where this makes a
  difference.

* Moreover notice that our goals here are different than the goals of
  the top-level overlapping checks. There we are interested in
  validating the following principle:

      If we inline a function f at a site where the same global
      instance environment is available as the instance environment at
      the definition site of f then we should get the same behaviour.

  But for the Given Overlap check our goal is just related to completeness of
  constraint solving.

* The solution is only a partial one.  Consider the above example with
       g :: forall a. Q [a] => [a] -> Int
       g x = let v = wob x
             in v
  and suppose we have -XNoMonoLocalBinds, so that we attempt to find the most
  general type for 'v'.  When generalising v's type we'll simplify its
  Q [alpha] constraint, but we don't have Q [a] in the 'givens', so we
  will use the instance declaration after all. #11948 was a case
  in point.

All of this is disgustingly delicate, so to discourage people from writing
simplifiable class givens, we warn about signatures that contain them;
see GHC.Tc.Validity Note [Simplifiable given constraints].

Note [Naturally coherent classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A few built-in classes are "naturally coherent".  This term means that
the "instance" for the class is bidirectional with its superclass(es).
For example, consider (~~), which behaves as if it was defined like
this:
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
(See Note [The equality types story] in GHC.Builtin.Types.Prim.)

Faced with [W] t1 ~~ t2, it's always OK to reduce it to [W] t1 ~# t2,
without worrying about Note [Instance and Given overlap].  Why?  Because
if we had [G] s1 ~~ s2, then we'd get the superclass [G] s1 ~# s2, and
so the reduction of the [W] constraint does not risk losing any solutions.

On the other hand, it can be fatal to /fail/ to reduce such
equalities, on the grounds of Note [Instance and Given overlap],
because many good things flow from [W] t1 ~# t2.

The same reasoning applies to

* (~~)        heqTyCOn
* (~)         eqTyCon
* Coercible   coercibleTyCon

And less obviously to:

* Tuple classes.  For reasons described in GHC.Tc.Solver.Monad
  Note [Tuples hiding implicit parameters], we may have a constraint
     [W] (?x::Int, C a)
  with an exactly-matching Given constraint.  We must decompose this
  tuple and solve the components separately, otherwise we won't solve
  it at all!  It is perfectly safe to decompose it, because again the
  superclasses invert the instance;  e.g.
      class (c1, c2) => (% c1, c2 %)
      instance (c1, c2) => (% c1, c2 %)
  Example in #14218

Exammples: T5853, T10432, T5315, T9222, T2627b, T3028b

PS: the term "naturally coherent" doesn't really seem helpful.
Perhaps "invertible" or something?  I left it for now though.

Note [Local instances and incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall b c. (Eq b, forall a. Eq a => Eq (c a))
                 => c b -> Bool
   f x = x==x

We get [W] Eq (c b), and we must use the local instance to solve it.

BUT that wanted also unifies with the top-level Eq [a] instance,
and Eq (Maybe a) etc.  We want the local instance to "win", otherwise
we can't solve the wanted at all.  So we mark it as Incohherent.
According to Note [Rules for instance lookup] in GHC.Core.InstEnv, that'll
make it win even if there are other instances that unify.

Moreover this is not a hack!  The evidence for this local instance
will be constructed by GHC at a call site... from the very instances
that unify with it here.  It is not like an incoherent user-written
instance which might have utterly different behaviour.

Consdider  f :: Eq a => blah.  If we have [W] Eq a, we certainly
get it from the Eq a context, without worrying that there are
lots of top-level instances that unify with [W] Eq a!  We'll use
those instances to build evidence to pass to f. That's just the
nullary case of what's happening here.
-}

matchLocalInst :: TcPredType -> CtLoc -> TcS ClsInstResult
-- Look up the predicate in Given quantified constraints,
-- which are effectively just local instance declarations.
matchLocalInst pred loc
  = do { ics <- getInertCans
       ; case match_local_inst (inert_insts ics) of
           ([], False) -> do { traceTcS "No local instance for" (ppr pred)
                             ; return NoInstance }
           ([(dfun_ev, inst_tys)], unifs)
             | not unifs
             -> do { let dfun_id = ctEvEvId dfun_ev
                   ; (tys, theta) <- instDFunType dfun_id inst_tys
                   ; let result = OneInst { cir_new_theta = theta
                                          , cir_mk_ev     = evDFunApp dfun_id tys
                                          , cir_what      = LocalInstance }
                   ; traceTcS "Local inst found:" (ppr result)
                   ; return result }
           _ -> do { traceTcS "Multiple local instances for" (ppr pred)
                   ; return NotSure }}
  where
    pred_tv_set = tyCoVarsOfType pred

    match_local_inst :: [QCInst]
                     -> ( [(CtEvidence, [DFunInstType])]
                        , Bool )      -- True <=> Some unify but do not match
    match_local_inst []
      = ([], False)
    match_local_inst (qci@(QCI { qci_tvs = qtvs, qci_pred = qpred
                               , qci_ev = ev })
                     : qcis)
      | let in_scope = mkInScopeSet (qtv_set `unionVarSet` pred_tv_set)
      , Just tv_subst <- ruleMatchTyKiX qtv_set (mkRnEnv2 in_scope)
                                        emptyTvSubstEnv qpred pred
      , let match = (ev, map (lookupVarEnv tv_subst) qtvs)
      = (match:matches, unif)

      | otherwise
      = ASSERT2( disjointVarSet qtv_set (tyCoVarsOfType pred)
               , ppr qci $$ ppr pred )
            -- ASSERT: unification relies on the
            -- quantified variables being fresh
        (matches, unif || this_unif)
      where
        qtv_set = mkVarSet qtvs
        this_unif = mightMatchLater qpred (ctEvLoc ev) pred loc
        (matches, unif) = match_local_inst qcis
