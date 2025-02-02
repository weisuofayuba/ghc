{-# LANGUAGE CPP, NondecreasingIndentation, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2012
--
-- The GHC API
--
-- -----------------------------------------------------------------------------

module GHC (
        -- * Initialisation
        defaultErrorHandler,
        defaultCleanupHandler,
        prettyPrintGhcErrors,
        withSignalHandlers,
        withCleanupSession,

        -- * GHC Monad
        Ghc, GhcT, GhcMonad(..), HscEnv,
        runGhc, runGhcT, initGhcMonad,
        printException,
        handleSourceError,
        needsTemplateHaskellOrQQ,

        -- * Flags and settings
        DynFlags(..), GeneralFlag(..), Severity(..), HscTarget(..), gopt,
        GhcMode(..), GhcLink(..), defaultObjectTarget,
        parseDynamicFlags,
        getSessionDynFlags, setSessionDynFlags,
        getProgramDynFlags, setProgramDynFlags, setLogAction,
        getInteractiveDynFlags, setInteractiveDynFlags,
        interpretPackageEnv,

        -- * Targets
        Target(..), TargetId(..), Phase,
        setTargets,
        getTargets,
        addTarget,
        removeTarget,
        guessTarget,

        -- * Loading\/compiling the program
        depanal, depanalE,
        load, LoadHowMuch(..), InteractiveImport(..),
        SuccessFlag(..), succeeded, failed,
        defaultWarnErrLogger, WarnErrLogger,
        workingDirectoryChanged,
        parseModule, typecheckModule, desugarModule, loadModule,
        ParsedModule(..), TypecheckedModule(..), DesugaredModule(..),
        TypecheckedSource, ParsedSource, RenamedSource,   -- ditto
        TypecheckedMod, ParsedMod,
        moduleInfo, renamedSource, typecheckedSource,
        parsedSource, coreModule,

        -- ** Compiling to Core
        CoreModule(..),
        compileToCoreModule, compileToCoreSimplified,

        -- * Inspecting the module structure of the program
        ModuleGraph, emptyMG, mapMG, mkModuleGraph, mgModSummaries,
        mgLookupModule,
        ModSummary(..), ms_mod_name, ModLocation(..),
        getModSummary,
        getModuleGraph,
        isLoaded,
        topSortModuleGraph,

        -- * Inspecting modules
        ModuleInfo,
        getModuleInfo,
        modInfoTyThings,
        modInfoTopLevelScope,
        modInfoExports,
        modInfoExportsWithSelectors,
        modInfoInstances,
        modInfoIsExportedName,
        modInfoLookupName,
        modInfoIface,
        modInfoRdrEnv,
        modInfoSafe,
        lookupGlobalName,
        findGlobalAnns,
        mkPrintUnqualifiedForModule,
        ModIface, ModIface_(..),
        SafeHaskellMode(..),

        -- * Querying the environment
        -- packageDbModules,

        -- * Printing
        PrintUnqualified, alwaysQualify,

        -- * Interactive evaluation

        -- ** Executing statements
        execStmt, execStmt', ExecOptions(..), execOptions, ExecResult(..),
        resumeExec,

        -- ** Adding new declarations
        runDecls, runDeclsWithLocation, runParsedDecls,

        -- ** Get/set the current context
        parseImportDecl,
        setContext, getContext,
        setGHCiMonad, getGHCiMonad,

        -- ** Inspecting the current context
        getBindings, getInsts, getPrintUnqual,
        findModule, lookupModule,
        isModuleTrusted, moduleTrustReqs,
        getNamesInScope,
        getRdrNamesInScope,
        getGRE,
        moduleIsInterpreted,
        getInfo,
        showModule,
        moduleIsBootOrNotObjectLinkable,
        getNameToInstancesIndex,

        -- ** Inspecting types and kinds
        exprType, TcRnExprMode(..),
        typeKind,

        -- ** Looking up a Name
        parseName,
        lookupName,

        -- ** Compiling expressions
        HValue, parseExpr, compileParsedExpr,
        GHC.Runtime.Eval.compileExpr, dynCompileExpr,
        ForeignHValue,
        compileExprRemote, compileParsedExprRemote,

        -- ** Docs
        getDocs, GetDocsFailure(..),

        -- ** Other
        runTcInteractive,   -- Desired by some clients (#8878)
        isStmt, hasImport, isImport, isDecl,

        -- ** The debugger
        SingleStep(..),
        Resume(..),
        History(historyBreakInfo, historyEnclosingDecls),
        GHC.getHistorySpan, getHistoryModule,
        abandon, abandonAll,
        getResumeContext,
        GHC.obtainTermFromId, GHC.obtainTermFromVal, reconstructType,
        modInfoModBreaks,
        ModBreaks(..), BreakIndex,
        BreakInfo(breakInfo_number, breakInfo_module),
        GHC.Runtime.Eval.back,
        GHC.Runtime.Eval.forward,

        -- * Abstract syntax elements

        -- ** Units
        Unit,

        -- ** Modules
        Module, mkModule, pprModule, moduleName, moduleUnit,
        ModuleName, mkModuleName, moduleNameString,

        -- ** Names
        Name,
        isExternalName, nameModule, pprParenSymName, nameSrcSpan,
        NamedThing(..),
        RdrName(Qual,Unqual),

        -- ** Identifiers
        Id, idType,
        isImplicitId, isDeadBinder,
        isExportedId, isLocalId, isGlobalId,
        isRecordSelector,
        isPrimOpId, isFCallId, isClassOpId_maybe,
        isDataConWorkId, idDataCon,
        isDeadEndId, isDictonaryId,
        recordSelectorTyCon,

        -- ** Type constructors
        TyCon,
        tyConTyVars, tyConDataCons, tyConArity,
        isClassTyCon, isTypeSynonymTyCon, isTypeFamilyTyCon, isNewTyCon,
        isPrimTyCon, isFunTyCon,
        isFamilyTyCon, isOpenFamilyTyCon, isOpenTypeFamilyTyCon,
        tyConClass_maybe,
        synTyConRhs_maybe, synTyConDefn_maybe, tyConKind,

        -- ** Type variables
        TyVar,
        alphaTyVars,

        -- ** Data constructors
        DataCon,
        dataConType, dataConTyCon, dataConFieldLabels,
        dataConIsInfix, isVanillaDataCon, dataConWrapperType,
        dataConSrcBangs,
        StrictnessMark(..), isMarkedStrict,

        -- ** Classes
        Class,
        classMethods, classSCTheta, classTvsFds, classATs,
        pprFundeps,

        -- ** Instances
        ClsInst,
        instanceDFunId,
        pprInstance, pprInstanceHdr,
        pprFamInst,

        FamInst,

        -- ** Types and Kinds
        Type, splitForAllTys, funResultTy,
        pprParendType, pprTypeApp,
        Kind,
        PredType,
        ThetaType, pprForAll, pprThetaArrowTy,
        parseInstanceHead,
        getInstancesForType,

        -- ** Entities
        TyThing(..),

        -- ** Syntax
        module GHC.Hs, -- ToDo: remove extraneous bits

        -- ** Fixities
        FixityDirection(..),
        defaultFixity, maxPrecedence,
        negateFixity,
        compareFixity,
        LexicalFixity(..),

        -- ** Source locations
        SrcLoc(..), RealSrcLoc,
        mkSrcLoc, noSrcLoc,
        srcLocFile, srcLocLine, srcLocCol,
        SrcSpan(..), RealSrcSpan,
        mkSrcSpan, srcLocSpan, isGoodSrcSpan, noSrcSpan,
        srcSpanStart, srcSpanEnd,
        srcSpanFile,
        srcSpanStartLine, srcSpanEndLine,
        srcSpanStartCol, srcSpanEndCol,

        -- ** Located
        GenLocated(..), Located, RealLocated,

        -- *** Constructing Located
        noLoc, mkGeneralLocated,

        -- *** Deconstructing Located
        getLoc, unLoc,
        getRealSrcSpan, unRealSrcSpan,

        -- *** Combining and comparing Located values
        eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost_smallest,
        spans, isSubspanOf,

        -- * Exceptions
        GhcException(..), showGhcException,

        -- * Token stream manipulations
        Token,
        getTokenStream, getRichTokenStream,
        showRichTokenStream, addSourceToTokens,

        -- * Pure interface to the parser
        parser,

        -- * API Annotations
        ApiAnns(..),AnnKeywordId(..),AnnotationComment(..), ApiAnnKey,
        getAnnotation, getAndRemoveAnnotation,
        getAnnotationComments, getAndRemoveAnnotationComments,
        unicodeAnn,

        -- * Miscellaneous
        --sessionHscEnv,
        cyclicModuleErr,
  ) where

{-
 ToDo:

  * inline bits of GHC.Driver.Main here to simplify layering: hscTcExpr, hscStmt.
-}

#include "HsVersions.h"

import GHC.Prelude hiding (init)

import GHC.ByteCode.Types
import GHC.Runtime.Eval
import GHC.Runtime.Eval.Types
import GHC.Runtime.Interpreter
import GHC.Runtime.Interpreter.Types
import GHCi.RemoteTypes

import GHC.Core.Ppr.TyThing  ( pprFamInst )
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Driver.Hooks
import GHC.Driver.Pipeline   ( compileOne' )
import GHC.Driver.Monad
import GHC.Tc.Utils.Monad    ( finalSafeMode, fixSafeInstances, initIfaceTcRn )
import GHC.Iface.Load        ( loadSysInterface )
import GHC.Tc.Types
import GHC.Core.Predicate
import GHC.Unit.State
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Hs
import GHC.Core.Type  hiding( typeKind )
import GHC.Tc.Utils.TcType
import GHC.Types.Id
import GHC.Builtin.Types.Prim ( alphaTyVars )
import GHC.Core.TyCon
import GHC.Core.TyCo.Ppr   ( pprForAll )
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.FVs        ( orphNamesOfFamInst )
import GHC.Core.FamInstEnv ( FamInst, famInstEnvElts )
import GHC.Core.InstEnv
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Avail
import GHC.Types.SrcLoc
import GHC.Core
import GHC.Iface.Tidy
import GHC.Driver.Phases   ( Phase(..), isHaskellSrcFilename )
import GHC.Driver.Finder
import GHC.Driver.Types
import GHC.Driver.CmdLine
import GHC.Driver.Session hiding (WarnReason(..))
import GHC.Driver.Ways
import GHC.SysTools
import GHC.SysTools.BaseDir
import GHC.Types.Annotations
import GHC.Unit.Module
import GHC.Utils.Panic
import GHC.Platform
import GHC.Data.Bag        ( listToBag )
import GHC.Utils.Error
import GHC.Utils.Monad
import GHC.Utils.Misc
import GHC.Data.StringBuffer
import GHC.Utils.Outputable
import GHC.Types.Basic
import GHC.Data.FastString
import qualified GHC.Parser as Parser
import GHC.Parser.Lexer
import GHC.Parser.Annotation
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Name.Env
import GHC.Tc.Module
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family
import GHC.SysTools.FileCleanup

import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Time
import Data.Typeable    ( Typeable )
import Data.Word        ( Word8 )
import Control.Monad
import System.Exit      ( exitWith, ExitCode(..) )
import GHC.Utils.Exception
import Data.IORef
import System.FilePath
import Control.Concurrent
import Control.Applicative ((<|>))
import Control.Monad.Catch as MC

import GHC.Data.Maybe
import System.IO.Error  ( isDoesNotExistError )
import System.Environment ( getEnv )
import System.Directory


-- %************************************************************************
-- %*                                                                      *
--             Initialisation: exception handlers
-- %*                                                                      *
-- %************************************************************************


-- | Install some default exception handlers and run the inner computation.
-- Unless you want to handle exceptions yourself, you should wrap this around
-- the top level of your program.  The default handlers output the error
-- message(s) to stderr and exit cleanly.
defaultErrorHandler :: (ExceptionMonad m)
                    => FatalMessager -> FlushOut -> m a -> m a
defaultErrorHandler fm (FlushOut flushOut) inner =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  MC.handle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg'' fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt ->
                         -- Important to let this one propagate out so our
                         -- calling process knows we were interrupted by ^C
                         liftIO $ throwIO UserInterrupt
                     Just StackOverflow ->
                         fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ ->
                              fatalErrorMsg'' fm
                                  (show (Panic (show exception)))
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
                flushOut
                case ge of
                     Signal _ -> exitWith (ExitFailure 1)
                     _ -> do fatalErrorMsg'' fm (show ge)
                             exitWith (ExitFailure 1)
            ) $
  inner

-- | This function is no longer necessary, cleanup is now done by
-- runGhc/runGhcT.
{-# DEPRECATED defaultCleanupHandler "Cleanup is now done by runGhc/runGhcT" #-}
defaultCleanupHandler :: (ExceptionMonad m) => DynFlags -> m a -> m a
defaultCleanupHandler _ m = m
 where _warning_suppression = m `MC.onException` undefined


-- %************************************************************************
-- %*                                                                      *
--             The Ghc Monad
-- %*                                                                      *
-- %************************************************************************

-- | Run function for the 'Ghc' monad.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.
--
-- Any errors not handled inside the 'Ghc' action are propagated as IO
-- exceptions.

runGhc :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
       -> Ghc a           -- ^ The action to perform.
       -> IO a
runGhc mb_top_dir ghc = do
  ref <- newIORef (panic "empty session")
  let session = Session ref
  flip unGhc session $ withSignalHandlers $ do -- catch ^C
    initGhcMonad mb_top_dir
    withCleanupSession ghc

-- | Run function for 'GhcT' monad transformer.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.

runGhcT :: ExceptionMonad m =>
           Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
        -> GhcT m a        -- ^ The action to perform.
        -> m a
runGhcT mb_top_dir ghct = do
  ref <- liftIO $ newIORef (panic "empty session")
  let session = Session ref
  flip unGhcT session $ withSignalHandlers $ do -- catch ^C
    initGhcMonad mb_top_dir
    withCleanupSession ghct

withCleanupSession :: GhcMonad m => m a -> m a
withCleanupSession ghc = ghc `MC.finally` cleanup
  where
   cleanup = do
      hsc_env <- getSession
      let dflags = hsc_dflags hsc_env
      liftIO $ do
          cleanTempFiles dflags
          cleanTempDirs dflags
          stopInterp hsc_env -- shut down the IServ
          --  exceptions will be blocked while we clean the temporary files,
          -- so there shouldn't be any difficulty if we receive further
          -- signals.

-- | Initialise a GHC session.
--
-- If you implement a custom 'GhcMonad' you must call this function in the
-- monad run function.  It will initialise the session variable and clear all
-- warnings.
--
-- The first argument should point to the directory where GHC's library files
-- reside.  More precisely, this should be the output of @ghc --print-libdir@
-- of the version of GHC the module using this API is compiled with.  For
-- portability, you should use the @ghc-paths@ package, available at
-- <http://hackage.haskell.org/package/ghc-paths>.

initGhcMonad :: GhcMonad m => Maybe FilePath -> m ()
initGhcMonad mb_top_dir
  = do { env <- liftIO $
                do { top_dir <- findTopDir mb_top_dir
                   ; mySettings <- initSysTools top_dir
                   ; myLlvmConfig <- lazyInitLlvmConfig top_dir
                   ; dflags <- initDynFlags (defaultDynFlags mySettings myLlvmConfig)
                   ; checkBrokenTablesNextToCode dflags
                   ; setUnsafeGlobalDynFlags dflags
                      -- c.f. DynFlags.parseDynamicFlagsFull, which
                      -- creates DynFlags and sets the UnsafeGlobalDynFlags
                   ; newHscEnv dflags }
       ; setSession env }

-- | The binutils linker on ARM emits unnecessary R_ARM_COPY relocations which
-- breaks tables-next-to-code in dynamically linked modules. This
-- check should be more selective but there is currently no released
-- version where this bug is fixed.
-- See https://sourceware.org/bugzilla/show_bug.cgi?id=16177 and
-- https://gitlab.haskell.org/ghc/ghc/issues/4210#note_78333
checkBrokenTablesNextToCode :: MonadIO m => DynFlags -> m ()
checkBrokenTablesNextToCode dflags
  = do { broken <- checkBrokenTablesNextToCode' dflags
       ; when broken
         $ do { _ <- liftIO $ throwIO $ mkApiErr dflags invalidLdErr
              ; liftIO $ fail "unsupported linker"
              }
       }
  where
    invalidLdErr = text "Tables-next-to-code not supported on ARM" <+>
                   text "when using binutils ld (please see:" <+>
                   text "https://sourceware.org/bugzilla/show_bug.cgi?id=16177)"

checkBrokenTablesNextToCode' :: MonadIO m => DynFlags -> m Bool
checkBrokenTablesNextToCode' dflags
  | not (isARM arch)                 = return False
  | WayDyn `S.notMember` ways dflags = return False
  | not tablesNextToCode             = return False
  | otherwise                        = do
    linkerInfo <- liftIO $ getLinkerInfo dflags
    case linkerInfo of
      GnuLD _  -> return True
      _        -> return False
  where platform = targetPlatform dflags
        arch = platformArch platform
        tablesNextToCode = platformTablesNextToCode platform


-- %************************************************************************
-- %*                                                                      *
--             Flags & settings
-- %*                                                                      *
-- %************************************************************************

-- $DynFlags
--
-- The GHC session maintains two sets of 'DynFlags':
--
--   * The "interactive" @DynFlags@, which are used for everything
--     related to interactive evaluation, including 'runStmt',
--     'runDecls', 'exprType', 'lookupName' and so on (everything
--     under \"Interactive evaluation\" in this module).
--
--   * The "program" @DynFlags@, which are used when loading
--     whole modules with 'load'
--
-- 'setInteractiveDynFlags', 'getInteractiveDynFlags' work with the
-- interactive @DynFlags@.
--
-- 'setProgramDynFlags', 'getProgramDynFlags' work with the
-- program @DynFlags@.
--
-- 'setSessionDynFlags' sets both @DynFlags@, and 'getSessionDynFlags'
-- retrieves the program @DynFlags@ (for backwards compatibility).


-- | Updates both the interactive and program DynFlags in a Session.
-- This also reads the package database (unless it has already been
-- read), and prepares the compilers knowledge about packages.  It can
-- be called again to load new packages: just add new package flags to
-- (packageFlags dflags).
--
-- Returns a list of new packages that may need to be linked in using
-- the dynamic linker (see 'linkPackages') as a result of new package
-- flags.  If you are not doing linking or doing static linking, you
-- can ignore the list of packages returned.
--
setSessionDynFlags :: GhcMonad m => DynFlags -> m ()
setSessionDynFlags dflags = do
  dflags' <- checkNewDynFlags dflags
  dflags''' <- liftIO $ initUnits dflags'

  -- Interpreter
  interp  <- if gopt Opt_ExternalInterpreter dflags
    then do
         let
           prog = pgm_i dflags ++ flavour
           profiled = ways dflags `hasWay` WayProf
           dynamic  = ways dflags `hasWay` WayDyn
           flavour
             | profiled  = "-prof" -- FIXME: can't we have both?
             | dynamic   = "-dyn"
             | otherwise = ""
           msg = text "Starting " <> text prog
         tr <- if verbosity dflags >= 3
                then return (logInfo dflags $ withPprStyle defaultDumpStyle msg)
                else return (pure ())
         let
          conf = IServConfig
            { iservConfProgram  = prog
            , iservConfOpts     = getOpts dflags opt_i
            , iservConfProfiled = profiled
            , iservConfDynamic  = dynamic
            , iservConfHook     = createIservProcessHook (hooks dflags)
            , iservConfTrace    = tr
            }
         s <- liftIO $ newMVar IServPending
         return (Just (ExternalInterp conf (IServ s)))
    else
#if defined(HAVE_INTERNAL_INTERPRETER)
      return (Just InternalInterp)
#else
      return Nothing
#endif

  modifySession $ \h -> h{ hsc_dflags = dflags'''
                         , hsc_IC = (hsc_IC h){ ic_dflags = dflags''' }
                         , hsc_interp = hsc_interp h <|> interp
                           -- we only update the interpreter if there wasn't
                           -- already one set up
                         }
  invalidateModSummaryCache

-- | Sets the program 'DynFlags'.  Note: this invalidates the internal
-- cached module graph, causing more work to be done the next time
-- 'load' is called.
--
-- Returns a boolean indicating if preload units have changed and need to be
-- reloaded.
setProgramDynFlags :: GhcMonad m => DynFlags -> m Bool
setProgramDynFlags dflags = setProgramDynFlags_ True dflags

-- | Set the action taken when the compiler produces a message.  This
-- can also be accomplished using 'setProgramDynFlags', but using
-- 'setLogAction' avoids invalidating the cached module graph.
setLogAction :: GhcMonad m => LogAction -> m ()
setLogAction action = do
  dflags' <- getProgramDynFlags
  void $ setProgramDynFlags_ False $
    dflags' { log_action = action }

setProgramDynFlags_ :: GhcMonad m => Bool -> DynFlags -> m Bool
setProgramDynFlags_ invalidate_needed dflags = do
  dflags' <- checkNewDynFlags dflags
  dflags_prev <- getProgramDynFlags
  let changed = packageFlagsChanged dflags_prev dflags'
  dflags'' <- if changed
               then liftIO $ initUnits dflags'
               else return dflags'
  modifySession $ \h -> h{ hsc_dflags = dflags'' }
  when invalidate_needed $ invalidateModSummaryCache
  return changed


-- When changing the DynFlags, we want the changes to apply to future
-- loads, but without completely discarding the program.  But the
-- DynFlags are cached in each ModSummary in the hsc_mod_graph, so
-- after a change to DynFlags, the changes would apply to new modules
-- but not existing modules; this seems undesirable.
--
-- Furthermore, the GHC API client might expect that changing
-- log_action would affect future compilation messages, but for those
-- modules we have cached ModSummaries for, we'll continue to use the
-- old log_action.  This is definitely wrong (#7478).
--
-- Hence, we invalidate the ModSummary cache after changing the
-- DynFlags.  We do this by tweaking the date on each ModSummary, so
-- that the next downsweep will think that all the files have changed
-- and preprocess them again.  This won't necessarily cause everything
-- to be recompiled, because by the time we check whether we need to
-- recompile a module, we'll have re-summarised the module and have a
-- correct ModSummary.
--
invalidateModSummaryCache :: GhcMonad m => m ()
invalidateModSummaryCache =
  modifySession $ \h -> h { hsc_mod_graph = mapMG inval (hsc_mod_graph h) }
 where
  inval ms = ms { ms_hs_date = addUTCTime (-1) (ms_hs_date ms) }

-- | Returns the program 'DynFlags'.
getProgramDynFlags :: GhcMonad m => m DynFlags
getProgramDynFlags = getSessionDynFlags

-- | Set the 'DynFlags' used to evaluate interactive expressions.
-- Note: this cannot be used for changes to packages.  Use
-- 'setSessionDynFlags', or 'setProgramDynFlags' and then copy the
-- 'unitState' into the interactive @DynFlags@.
setInteractiveDynFlags :: GhcMonad m => DynFlags -> m ()
setInteractiveDynFlags dflags = do
  dflags' <- checkNewDynFlags dflags
  dflags'' <- checkNewInteractiveDynFlags dflags'
  modifySession $ \h -> h{ hsc_IC = (hsc_IC h) { ic_dflags = dflags'' }}

-- | Get the 'DynFlags' used to evaluate interactive expressions.
getInteractiveDynFlags :: GhcMonad m => m DynFlags
getInteractiveDynFlags = withSession $ \h -> return (ic_dflags (hsc_IC h))


parseDynamicFlags :: MonadIO m =>
                     DynFlags -> [Located String]
                  -> m (DynFlags, [Located String], [Warn])
parseDynamicFlags dflags cmdline = do
  (dflags1, leftovers, warns) <- parseDynamicFlagsCmdLine dflags cmdline
  dflags2 <- liftIO $ interpretPackageEnv dflags1
  return (dflags2, leftovers, warns)


-- | Checks the set of new DynFlags for possibly erroneous option
-- combinations when invoking 'setSessionDynFlags' and friends, and if
-- found, returns a fixed copy (if possible).
checkNewDynFlags :: MonadIO m => DynFlags -> m DynFlags
checkNewDynFlags dflags = do
  -- See Note [DynFlags consistency]
  let (dflags', warnings) = makeDynFlagsConsistent dflags
  liftIO $ handleFlagWarnings dflags (map (Warn NoReason) warnings)
  return dflags'

checkNewInteractiveDynFlags :: MonadIO m => DynFlags -> m DynFlags
checkNewInteractiveDynFlags dflags0 = do
  -- We currently don't support use of StaticPointers in expressions entered on
  -- the REPL. See #12356.
  if xopt LangExt.StaticPointers dflags0
  then do liftIO $ printOrThrowWarnings dflags0 $ listToBag
            [mkPlainWarnMsg dflags0 interactiveSrcSpan
             $ text "StaticPointers is not supported in GHCi interactive expressions."]
          return $ xopt_unset dflags0 LangExt.StaticPointers
  else return dflags0


-- %************************************************************************
-- %*                                                                      *
--             Setting, getting, and modifying the targets
-- %*                                                                      *
-- %************************************************************************

-- ToDo: think about relative vs. absolute file paths. And what
-- happens when the current directory changes.

-- | Sets the targets for this session.  Each target may be a module name
-- or a filename.  The targets correspond to the set of root modules for
-- the program\/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty, followed by 'load'.
setTargets :: GhcMonad m => [Target] -> m ()
setTargets targets = modifySession (\h -> h{ hsc_targets = targets })

-- | Returns the current set of targets
getTargets :: GhcMonad m => m [Target]
getTargets = withSession (return . hsc_targets)

-- | Add another target.
addTarget :: GhcMonad m => Target -> m ()
addTarget target
  = modifySession (\h -> h{ hsc_targets = target : hsc_targets h })

-- | Remove a target
removeTarget :: GhcMonad m => TargetId -> m ()
removeTarget target_id
  = modifySession (\h -> h{ hsc_targets = filter (hsc_targets h) })
  where
   filter targets = [ t | t@(Target id _ _) <- targets, id /= target_id ]

-- | Attempts to guess what Target a string refers to.  This function
-- implements the @--make@/GHCi command-line syntax for filenames:
--
--   - if the string looks like a Haskell source filename, then interpret it
--     as such
--
--   - if adding a .hs or .lhs suffix yields the name of an existing file,
--     then use that
--
--   - otherwise interpret the string as a module name
--
guessTarget :: GhcMonad m => String -> Maybe Phase -> m Target
guessTarget str (Just phase)
   = return (Target (TargetFile str (Just phase)) True Nothing)
guessTarget str Nothing
   | isHaskellSrcFilename file
   = return (target (TargetFile file Nothing))
   | otherwise
   = do exists <- liftIO $ doesFileExist hs_file
        if exists
           then return (target (TargetFile hs_file Nothing))
           else do
        exists <- liftIO $ doesFileExist lhs_file
        if exists
           then return (target (TargetFile lhs_file Nothing))
           else do
        if looksLikeModuleName file
           then return (target (TargetModule (mkModuleName file)))
           else do
        dflags <- getDynFlags
        liftIO $ throwGhcExceptionIO
                 (ProgramError (showSDoc dflags $
                 text "target" <+> quotes (text file) <+>
                 text "is not a module name or a source file"))
     where
         (file,obj_allowed)
                | '*':rest <- str = (rest, False)
                | otherwise       = (str,  True)

         hs_file  = file <.> "hs"
         lhs_file = file <.> "lhs"

         target tid = Target tid obj_allowed Nothing


-- | Inform GHC that the working directory has changed.  GHC will flush
-- its cache of module locations, since it may no longer be valid.
--
-- Note: Before changing the working directory make sure all threads running
-- in the same session have stopped.  If you change the working directory,
-- you should also unload the current program (set targets to empty,
-- followed by load).
workingDirectoryChanged :: GhcMonad m => m ()
workingDirectoryChanged = withSession $ (liftIO . flushFinderCaches)


-- %************************************************************************
-- %*                                                                      *
--             Running phases one at a time
-- %*                                                                      *
-- %************************************************************************

class ParsedMod m where
  modSummary   :: m -> ModSummary
  parsedSource :: m -> ParsedSource

class ParsedMod m => TypecheckedMod m where
  renamedSource     :: m -> Maybe RenamedSource
  typecheckedSource :: m -> TypecheckedSource
  moduleInfo        :: m -> ModuleInfo
  tm_internals      :: m -> (TcGblEnv, ModDetails)
        -- ToDo: improvements that could be made here:
        --  if the module succeeded renaming but not typechecking,
        --  we can still get back the GlobalRdrEnv and exports, so
        --  perhaps the ModuleInfo should be split up into separate
        --  fields.

class TypecheckedMod m => DesugaredMod m where
  coreModule :: m -> ModGuts

-- | The result of successful parsing.
data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource
               , pm_extra_src_files :: [FilePath]
               , pm_annotations :: ApiAnns }
               -- See Note [Api annotations] in GHC.Parser.Annotation

instance ParsedMod ParsedModule where
  modSummary m    = pm_mod_summary m
  parsedSource m = pm_parsed_source m

-- | The result of successful typechecking.  It also contains the parser
--   result.
data TypecheckedModule =
  TypecheckedModule { tm_parsed_module       :: ParsedModule
                    , tm_renamed_source      :: Maybe RenamedSource
                    , tm_typechecked_source  :: TypecheckedSource
                    , tm_checked_module_info :: ModuleInfo
                    , tm_internals_          :: (TcGblEnv, ModDetails)
                    }

instance ParsedMod TypecheckedModule where
  modSummary m   = modSummary (tm_parsed_module m)
  parsedSource m = parsedSource (tm_parsed_module m)

instance TypecheckedMod TypecheckedModule where
  renamedSource m     = tm_renamed_source m
  typecheckedSource m = tm_typechecked_source m
  moduleInfo m        = tm_checked_module_info m
  tm_internals m      = tm_internals_ m

-- | The result of successful desugaring (i.e., translation to core).  Also
--  contains all the information of a typechecked module.
data DesugaredModule =
  DesugaredModule { dm_typechecked_module :: TypecheckedModule
                  , dm_core_module        :: ModGuts
             }

instance ParsedMod DesugaredModule where
  modSummary m   = modSummary (dm_typechecked_module m)
  parsedSource m = parsedSource (dm_typechecked_module m)

instance TypecheckedMod DesugaredModule where
  renamedSource m     = renamedSource (dm_typechecked_module m)
  typecheckedSource m = typecheckedSource (dm_typechecked_module m)
  moduleInfo m        = moduleInfo (dm_typechecked_module m)
  tm_internals m      = tm_internals_ (dm_typechecked_module m)

instance DesugaredMod DesugaredModule where
  coreModule m = dm_core_module m

type ParsedSource      = Located HsModule
type RenamedSource     = (HsGroup GhcRn, [LImportDecl GhcRn], Maybe [(LIE GhcRn, Avails)],
                          Maybe LHsDocString)
type TypecheckedSource = LHsBinds GhcTc

-- NOTE:
--   - things that aren't in the output of the typechecker right now:
--     - the export list
--     - the imports
--     - type signatures
--     - type/data/newtype declarations
--     - class declarations
--     - instances
--   - extra things in the typechecker's output:
--     - default methods are turned into top-level decls.
--     - dictionary bindings

-- | Return the 'ModSummary' of a module with the given name.
--
-- The module must be part of the module graph (see 'hsc_mod_graph' and
-- 'ModuleGraph').  If this is not the case, this function will throw a
-- 'GhcApiError'.
--
-- This function ignores boot modules and requires that there is only one
-- non-boot module with the given name.
getModSummary :: GhcMonad m => ModuleName -> m ModSummary
getModSummary mod = do
   mg <- liftM hsc_mod_graph getSession
   let mods_by_name = [ ms | ms <- mgModSummaries mg
                      , ms_mod_name ms == mod
                      , isBootSummary ms == NotBoot ]
   case mods_by_name of
     [] -> do dflags <- getDynFlags
              liftIO $ throwIO $ mkApiErr dflags (text "Module not part of module graph")
     [ms] -> return ms
     multiple -> do dflags <- getDynFlags
                    liftIO $ throwIO $ mkApiErr dflags (text "getModSummary is ambiguous: " <+> ppr multiple)

-- | Parse a module.
--
-- Throws a 'SourceError' on parse error.
parseModule :: GhcMonad m => ModSummary -> m ParsedModule
parseModule ms = do
   hsc_env <- getSession
   let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
   hpm <- liftIO $ hscParse hsc_env_tmp ms
   return (ParsedModule ms (hpm_module hpm) (hpm_src_files hpm)
                           (hpm_annotations hpm))
               -- See Note [Api annotations] in GHC.Parser.Annotation

-- | Typecheck and rename a parsed module.
--
-- Throws a 'SourceError' if either fails.
typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
typecheckModule pmod = do
 let ms = modSummary pmod
 hsc_env <- getSession
 let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
 (tc_gbl_env, rn_info)
       <- liftIO $ hscTypecheckRename hsc_env_tmp ms $
                      HsParsedModule { hpm_module = parsedSource pmod,
                                       hpm_src_files = pm_extra_src_files pmod,
                                       hpm_annotations = pm_annotations pmod }
 details <- liftIO $ makeSimpleDetails hsc_env_tmp tc_gbl_env
 safe    <- liftIO $ finalSafeMode (ms_hspp_opts ms) tc_gbl_env

 return $
     TypecheckedModule {
       tm_internals_          = (tc_gbl_env, details),
       tm_parsed_module       = pmod,
       tm_renamed_source      = rn_info,
       tm_typechecked_source  = tcg_binds tc_gbl_env,
       tm_checked_module_info =
         ModuleInfo {
           minf_type_env  = md_types details,
           minf_exports   = md_exports details,
           minf_rdr_env   = Just (tcg_rdr_env tc_gbl_env),
           minf_instances = fixSafeInstances safe $ md_insts details,
           minf_iface     = Nothing,
           minf_safe      = safe,
           minf_modBreaks = emptyModBreaks
         }}

-- | Desugar a typechecked module.
desugarModule :: GhcMonad m => TypecheckedModule -> m DesugaredModule
desugarModule tcm = do
 let ms = modSummary tcm
 let (tcg, _) = tm_internals tcm
 hsc_env <- getSession
 let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
 guts <- liftIO $ hscDesugar hsc_env_tmp ms tcg
 return $
     DesugaredModule {
       dm_typechecked_module = tcm,
       dm_core_module        = guts
     }

-- | Load a module.  Input doesn't need to be desugared.
--
-- A module must be loaded before dependent modules can be typechecked.  This
-- always includes generating a 'ModIface' and, depending on the
-- @DynFlags@\'s 'GHC.Driver.Session.hscTarget', may also include code generation.
--
-- This function will always cause recompilation and will always overwrite
-- previous compilation results (potentially files on disk).
--
loadModule :: (TypecheckedMod mod, GhcMonad m) => mod -> m mod
loadModule tcm = do
   let ms = modSummary tcm
   let mod = ms_mod_name ms
   let loc = ms_location ms
   let (tcg, _details) = tm_internals tcm

   mb_linkable <- case ms_obj_date ms of
                     Just t | t > ms_hs_date ms  -> do
                         l <- liftIO $ findObjectLinkable (ms_mod ms)
                                                  (ml_obj_file loc) t
                         return (Just l)
                     _otherwise -> return Nothing

   let source_modified | isNothing mb_linkable = SourceModified
                       | otherwise             = SourceUnmodified
                       -- we can't determine stability here

   -- compile doesn't change the session
   hsc_env <- getSession
   mod_info <- liftIO $ compileOne' (Just tcg) Nothing
                                    hsc_env ms 1 1 Nothing mb_linkable
                                    source_modified

   modifySession $ \e -> e{ hsc_HPT = addToHpt (hsc_HPT e) mod mod_info }
   return tcm


-- %************************************************************************
-- %*                                                                      *
--             Dealing with Core
-- %*                                                                      *
-- %************************************************************************

-- | A CoreModule consists of just the fields of a 'ModGuts' that are needed for
-- the 'GHC.compileToCoreModule' interface.
data CoreModule
  = CoreModule {
      -- | Module name
      cm_module   :: !Module,
      -- | Type environment for types declared in this module
      cm_types    :: !TypeEnv,
      -- | Declarations
      cm_binds    :: CoreProgram,
      -- | Safe Haskell mode
      cm_safe     :: SafeHaskellMode
    }

instance Outputable CoreModule where
   ppr (CoreModule {cm_module = mn, cm_types = te, cm_binds = cb,
                    cm_safe = sf})
    = text "%module" <+> ppr mn <+> parens (ppr sf) <+> ppr te
      $$ vcat (map ppr cb)

-- | This is the way to get access to the Core bindings corresponding
-- to a module. 'compileToCore' parses, typechecks, and
-- desugars the module, then returns the resulting Core module (consisting of
-- the module name, type declarations, and function declarations) if
-- successful.
compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileToCoreModule = compileCore False

-- | Like compileToCoreModule, but invokes the simplifier, so
-- as to return simplified and tidied Core.
compileToCoreSimplified :: GhcMonad m => FilePath -> m CoreModule
compileToCoreSimplified = compileCore True

compileCore :: GhcMonad m => Bool -> FilePath -> m CoreModule
compileCore simplify fn = do
   -- First, set the target to the desired filename
   target <- guessTarget fn Nothing
   addTarget target
   _ <- load LoadAllTargets
   -- Then find dependencies
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) (mgModSummaries modGraph) of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
       (tcg, mod_guts) <- -- TODO: space leaky: call hsc* directly?
         do tm <- typecheckModule =<< parseModule modSummary
            let tcg = fst (tm_internals tm)
            (,) tcg . coreModule <$> desugarModule tm
       liftM (gutsToCoreModule (mg_safe_haskell mod_guts)) $
         if simplify
          then do
             -- If simplify is true: simplify (hscSimplify), then tidy
             -- (tidyProgram).
             hsc_env <- getSession
             simpl_guts <- liftIO $ do
               plugins <- readIORef (tcg_th_coreplugins tcg)
               hscSimplify hsc_env plugins mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts

     Nothing -> panic "compileToCoreModule: target FilePath not found in\
                           module dependency graph"
  where -- two versions, based on whether we simplify (thus run tidyProgram,
        -- which returns a (CgGuts, ModDetails) pair, or not (in which case
        -- we just have a ModGuts.
        gutsToCoreModule :: SafeHaskellMode
                         -> Either (CgGuts, ModDetails) ModGuts
                         -> CoreModule
        gutsToCoreModule safe_mode (Left (cg, md)) = CoreModule {
          cm_module = cg_module cg,
          cm_types  = md_types md,
          cm_binds  = cg_binds cg,
          cm_safe   = safe_mode
        }
        gutsToCoreModule safe_mode (Right mg) = CoreModule {
          cm_module  = mg_module mg,
          cm_types   = typeEnvFromEntities (bindersOfBinds (mg_binds mg))
                                           (mg_tcs mg)
                                           (mg_fam_insts mg),
          cm_binds   = mg_binds mg,
          cm_safe    = safe_mode
         }

-- %************************************************************************
-- %*                                                                      *
--             Inspecting the session
-- %*                                                                      *
-- %************************************************************************

-- | Get the module dependency graph.
getModuleGraph :: GhcMonad m => m ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph = liftM hsc_mod_graph getSession

-- | Return @True@ \<==> module is loaded.
isLoaded :: GhcMonad m => ModuleName -> m Bool
isLoaded m = withSession $ \hsc_env ->
  return $! isJust (lookupHpt (hsc_HPT hsc_env) m)

-- | Return the bindings for the current interactive session.
getBindings :: GhcMonad m => m [TyThing]
getBindings = withSession $ \hsc_env ->
    return $ icInScopeTTs $ hsc_IC hsc_env

-- | Return the instances for the current interactive session.
getInsts :: GhcMonad m => m ([ClsInst], [FamInst])
getInsts = withSession $ \hsc_env ->
    return $ ic_instances (hsc_IC hsc_env)

getPrintUnqual :: GhcMonad m => m PrintUnqualified
getPrintUnqual = withSession $ \hsc_env ->
  return (icPrintUnqual (hsc_dflags hsc_env) (hsc_IC hsc_env))

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
        minf_type_env  :: TypeEnv,
        minf_exports   :: [AvailInfo],
        minf_rdr_env   :: Maybe GlobalRdrEnv,   -- Nothing for a compiled/package mod
        minf_instances :: [ClsInst],
        minf_iface     :: Maybe ModIface,
        minf_safe      :: SafeHaskellMode,
        minf_modBreaks :: ModBreaks
  }
        -- We don't want HomeModInfo here, because a ModuleInfo applies
        -- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: GhcMonad m => Module -> m (Maybe ModuleInfo)  -- XXX: Maybe X
getModuleInfo mdl = withSession $ \hsc_env -> do
  let mg = hsc_mod_graph hsc_env
  if mgElemModule mg mdl
        then liftIO $ getHomeModuleInfo hsc_env mdl
        else do
  {- if isHomeModule (hsc_dflags hsc_env) mdl
        then return Nothing
        else -} liftIO $ getPackageModuleInfo hsc_env mdl
   -- ToDo: we don't understand what the following comment means.
   --    (SDM, 19/7/2011)
   -- getPackageModuleInfo will attempt to find the interface, so
   -- we don't want to call it for a home module, just in case there
   -- was a problem loading the module and the interface doesn't
   -- exist... hence the isHomeModule test here.  (ToDo: reinstate)

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getPackageModuleInfo hsc_env mdl
  = do  eps <- hscEPS hsc_env
        iface <- hscGetModuleInterface hsc_env mdl
        let
            avails = mi_exports iface
            pte    = eps_PTE eps
            tys    = [ ty | name <- concatMap availNames avails,
                            Just ty <- [lookupTypeEnv pte name] ]
        --
        return (Just (ModuleInfo {
                        minf_type_env  = mkTypeEnv tys,
                        minf_exports   = avails,
                        minf_rdr_env   = Just $! availsToGlobalRdrEnv (moduleName mdl) avails,
                        minf_instances = error "getModuleInfo: instances for package module unimplemented",
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface,
                        minf_modBreaks = emptyModBreaks
                }))

getHomeModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getHomeModuleInfo hsc_env mdl =
  case lookupHpt (hsc_HPT hsc_env) (moduleName mdl) of
    Nothing  -> return Nothing
    Just hmi -> do
      let details = hm_details hmi
          iface   = hm_iface hmi
      return (Just (ModuleInfo {
                        minf_type_env  = md_types details,
                        minf_exports   = md_exports details,
                        minf_rdr_env   = mi_globals $! hm_iface hmi,
                        minf_instances = md_insts details,
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface
                       ,minf_modBreaks = getModBreaks hmi
                        }))

-- | The list of top-level entities defined in a module
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (minf_type_env minf)

modInfoTopLevelScope :: ModuleInfo -> Maybe [Name]
modInfoTopLevelScope minf
  = fmap (map gre_name . globalRdrEnvElts) (minf_rdr_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = concatMap availNames $! minf_exports minf

modInfoExportsWithSelectors :: ModuleInfo -> [Name]
modInfoExportsWithSelectors minf = concatMap availNamesWithSelectors $! minf_exports minf

-- | Returns the instances defined by the specified module.
-- Warning: currently unimplemented for package modules.
modInfoInstances :: ModuleInfo -> [ClsInst]
modInfoInstances = minf_instances

modInfoIsExportedName :: ModuleInfo -> Name -> Bool
modInfoIsExportedName minf name = elemNameSet name (availsToNameSet (minf_exports minf))

mkPrintUnqualifiedForModule :: GhcMonad m =>
                               ModuleInfo
                            -> m (Maybe PrintUnqualified) -- XXX: returns a Maybe X
mkPrintUnqualifiedForModule minf = withSession $ \hsc_env -> do
  return (fmap (mkPrintUnqualified (hsc_dflags hsc_env)) (minf_rdr_env minf))

modInfoLookupName :: GhcMonad m =>
                     ModuleInfo -> Name
                  -> m (Maybe TyThing) -- XXX: returns a Maybe X
modInfoLookupName minf name = withSession $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> do
       eps <- liftIO $ readIORef (hsc_EPS hsc_env)
       return $! lookupType (hsc_dflags hsc_env)
                            (hsc_HPT hsc_env) (eps_PTE eps) name

modInfoIface :: ModuleInfo -> Maybe ModIface
modInfoIface = minf_iface

modInfoRdrEnv :: ModuleInfo -> Maybe GlobalRdrEnv
modInfoRdrEnv = minf_rdr_env

-- | Retrieve module safe haskell mode
modInfoSafe :: ModuleInfo -> SafeHaskellMode
modInfoSafe = minf_safe

modInfoModBreaks :: ModuleInfo -> ModBreaks
modInfoModBreaks = minf_modBreaks

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of {
      (_tvs, _theta, tau) -> isDictTy tau }

-- | Looks up a global name: that is, any top-level name in any
-- visible module.  Unlike 'lookupName', lookupGlobalName does not use
-- the interactive context, and therefore does not require a preceding
-- 'setContext'.
lookupGlobalName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupGlobalName name = withSession $ \hsc_env -> do
   liftIO $ lookupTypeHscEnv hsc_env name

findGlobalAnns :: (GhcMonad m, Typeable a) => ([Word8] -> a) -> AnnTarget Name -> m [a]
findGlobalAnns deserialize target = withSession $ \hsc_env -> do
    ann_env <- liftIO $ prepareAnnotations hsc_env Nothing
    return (findAnns deserialize ann_env target)

-- | get the GlobalRdrEnv for a session
getGRE :: GhcMonad m => m GlobalRdrEnv
getGRE = withSession $ \hsc_env-> return $ ic_rn_gbl_env (hsc_IC hsc_env)

-- | Retrieve all type and family instances in the environment, indexed
-- by 'Name'. Each name's lists will contain every instance in which that name
-- is mentioned in the instance head.
getNameToInstancesIndex :: GhcMonad m
  => [Module]        -- ^ visible modules. An orphan instance will be returned
                     -- if it is visible from at least one module in the list.
  -> Maybe [Module]  -- ^ modules to load. If this is not specified, we load
                     -- modules for everything that is in scope unqualified.
  -> m (Messages, Maybe (NameEnv ([ClsInst], [FamInst])))
getNameToInstancesIndex visible_mods mods_to_load = do
  hsc_env <- getSession
  liftIO $ runTcInteractive hsc_env $
    do { case mods_to_load of
           Nothing -> loadUnqualIfaces hsc_env (hsc_IC hsc_env)
           Just mods ->
             let doc = text "Need interface for reporting instances in scope"
             in initIfaceTcRn $ mapM_ (loadSysInterface doc) mods

       ; InstEnvs {ie_global, ie_local} <- tcGetInstEnvs
       ; let visible_mods' = mkModuleSet visible_mods
       ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
       -- We use Data.Sequence.Seq because we are creating left associated
       -- mappends.
       -- cls_index and fam_index below are adapted from GHC.Tc.Module.lookupInsts
       ; let cls_index = Map.fromListWith mappend
                 [ (n, Seq.singleton ispec)
                 | ispec <- instEnvElts ie_local ++ instEnvElts ie_global
                 , instIsVisible visible_mods' ispec
                 , n <- nameSetElemsStable $ orphNamesOfClsInst ispec
                 ]
       ; let fam_index = Map.fromListWith mappend
                 [ (n, Seq.singleton fispec)
                 | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
                 , n <- nameSetElemsStable $ orphNamesOfFamInst fispec
                 ]
       ; return $ mkNameEnv $
           [ (nm, (toList clss, toList fams))
           | (nm, (clss, fams)) <- Map.toList $ Map.unionWith mappend
               (fmap (,Seq.empty) cls_index)
               (fmap (Seq.empty,) fam_index)
           ] }

-- -----------------------------------------------------------------------------

{- ToDo: Move the primary logic here to "GHC.Unit.State"
-- | Return all /external/ modules available in the package database.
-- Modules from the current session (i.e., from the 'HomePackageTable') are
-- not included.  This includes module names which are reexported by packages.
packageDbModules :: GhcMonad m =>
                    Bool  -- ^ Only consider exposed packages.
                 -> m [Module]
packageDbModules only_exposed = do
   dflags <- getSessionDynFlags
   let pkgs = eltsUFM (unitInfoMap (unitState dflags))
   return $
     [ mkModule pid modname
     | p <- pkgs
     , not only_exposed || exposed p
     , let pid = mkUnit p
     , modname <- exposedModules p
               ++ map exportName (reexportedModules p) ]
               -}

-- -----------------------------------------------------------------------------
-- Misc exported utils

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

-- | print a 'NamedThing', adding parentheses if the name is an operator.
pprParenSymName :: NamedThing a => a -> SDoc
pprParenSymName a = parenSymOcc (getOccName a) (ppr (getName a))

-- ----------------------------------------------------------------------------


-- ToDo:
--   - Data and Typeable instances for HsSyn.

-- ToDo: check for small transformations that happen to the syntax in
-- the typechecker (eg. -e ==> negate e, perhaps for fromIntegral)

-- ToDo: maybe use TH syntax instead of Iface syntax?  There's already a way
-- to get from TyCons, Ids etc. to TH syntax (reify).

-- :browse will use either lm_toplev or inspect lm_interface, depending
-- on whether the module is interpreted or not.


-- Extract the filename, stringbuffer content and dynflags associed to a module
--
-- XXX: Explain pre-conditions
getModuleSourceAndFlags :: GhcMonad m => Module -> m (String, StringBuffer, DynFlags)
getModuleSourceAndFlags mod = do
  m <- getModSummary (moduleName mod)
  case ml_hs_file $ ms_location m of
    Nothing -> do dflags <- getDynFlags
                  liftIO $ throwIO $ mkApiErr dflags (text "No source available for module " <+> ppr mod)
    Just sourceFile -> do
        source <- liftIO $ hGetStringBuffer sourceFile
        return (sourceFile, source, ms_hspp_opts m)


-- | Return module source as token stream, including comments.
--
-- The module must be in the module graph and its source must be available.
-- Throws a 'GHC.Driver.Types.SourceError' on parse error.
getTokenStream :: GhcMonad m => Module -> m [Located Token]
getTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkRealSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts  -> return ts
    PFailed pst ->
        do dflags <- getDynFlags
           throwErrors (getErrorMessages pst dflags)

-- | Give even more information on the source than 'getTokenStream'
-- This function allows reconstructing the source completely with
-- 'showRichTokenStream'.
getRichTokenStream :: GhcMonad m => Module -> m [(Located Token, String)]
getRichTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkRealSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts -> return $ addSourceToTokens startLoc source ts
    PFailed pst ->
        do dflags <- getDynFlags
           throwErrors (getErrorMessages pst dflags)

-- | Given a source location and a StringBuffer corresponding to this
-- location, return a rich token stream with the source associated to the
-- tokens.
addSourceToTokens :: RealSrcLoc -> StringBuffer -> [Located Token]
                  -> [(Located Token, String)]
addSourceToTokens _ _ [] = []
addSourceToTokens loc buf (t@(L span _) : ts)
    = case span of
      UnhelpfulSpan _ -> (t,"") : addSourceToTokens loc buf ts
      RealSrcSpan s _ -> (t,str) : addSourceToTokens newLoc newBuf ts
        where
          (newLoc, newBuf, str) = go "" loc buf
          start = realSrcSpanStart s
          end = realSrcSpanEnd s
          go acc loc buf | loc < start = go acc nLoc nBuf
                         | start <= loc && loc < end = go (ch:acc) nLoc nBuf
                         | otherwise = (loc, buf, reverse acc)
              where (ch, nBuf) = nextChar buf
                    nLoc = advanceSrcLoc loc ch


-- | Take a rich token stream such as produced from 'getRichTokenStream' and
-- return source code almost identical to the original code (except for
-- insignificant whitespace.)
showRichTokenStream :: [(Located Token, String)] -> String
showRichTokenStream ts = go startLoc ts ""
    where sourceFile = getFile $ map (getLoc . fst) ts
          getFile [] = panic "showRichTokenStream: No source file found"
          getFile (UnhelpfulSpan _ : xs) = getFile xs
          getFile (RealSrcSpan s _ : _) = srcSpanFile s
          startLoc = mkRealSrcLoc sourceFile 1 1
          go _ [] = id
          go loc ((L span _, str):ts)
              = case span of
                UnhelpfulSpan _ -> go loc ts
                RealSrcSpan s _
                 | locLine == tokLine -> ((replicate (tokCol - locCol) ' ') ++)
                                       . (str ++)
                                       . go tokEnd ts
                 | otherwise -> ((replicate (tokLine - locLine) '\n') ++)
                               . ((replicate (tokCol - 1) ' ') ++)
                              . (str ++)
                              . go tokEnd ts
                  where (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                        (tokLine, tokCol) = (srcSpanStartLine s, srcSpanStartCol s)
                        tokEnd = realSrcSpanEnd s

-- -----------------------------------------------------------------------------
-- Interactive evaluation

-- | Takes a 'ModuleName' and possibly a 'UnitId', and consults the
-- filesystem and package database to find the corresponding 'Module',
-- using the algorithm that is used for an @import@ declaration.
findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
findModule mod_name maybe_pkg = withSession $ \hsc_env -> do
  let
    dflags   = hsc_dflags hsc_env
    this_pkg = homeUnit dflags
  --
  case maybe_pkg of
    Just pkg | fsToUnit pkg /= this_pkg && pkg /= fsLit "this" -> liftIO $ do
      res <- findImportedModule hsc_env mod_name maybe_pkg
      case res of
        Found _ m -> return m
        err       -> throwOneError $ noModError dflags noSrcSpan mod_name err
    _otherwise -> do
      home <- lookupLoadedHomeModule mod_name
      case home of
        Just m  -> return m
        Nothing -> liftIO $ do
           res <- findImportedModule hsc_env mod_name maybe_pkg
           case res of
             Found loc m | moduleUnit m /= this_pkg -> return m
                         | otherwise -> modNotLoadedError dflags m loc
             err -> throwOneError $ noModError dflags noSrcSpan mod_name err

modNotLoadedError :: DynFlags -> Module -> ModLocation -> IO a
modNotLoadedError dflags m loc = throwGhcExceptionIO $ CmdLineError $ showSDoc dflags $
   text "module is not loaded:" <+>
   quotes (ppr (moduleName m)) <+>
   parens (text (expectJust "modNotLoadedError" (ml_hs_file loc)))

-- | Like 'findModule', but differs slightly when the module refers to
-- a source file, and the file has not been loaded via 'load'.  In
-- this case, 'findModule' will throw an error (module not loaded),
-- but 'lookupModule' will check to see whether the module can also be
-- found in a package, and if so, that package 'Module' will be
-- returned.  If not, the usual module-not-found error will be thrown.
--
lookupModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
lookupModule mod_name (Just pkg) = findModule mod_name (Just pkg)
lookupModule mod_name Nothing = withSession $ \hsc_env -> do
  home <- lookupLoadedHomeModule mod_name
  case home of
    Just m  -> return m
    Nothing -> liftIO $ do
      res <- findExposedPackageModule hsc_env mod_name Nothing
      case res of
        Found _ m -> return m
        err       -> throwOneError $ noModError (hsc_dflags hsc_env) noSrcSpan mod_name err

lookupLoadedHomeModule :: GhcMonad m => ModuleName -> m (Maybe Module)
lookupLoadedHomeModule mod_name = withSession $ \hsc_env ->
  case lookupHpt (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (Just (mi_module (hm_iface mod_info)))
    _not_a_home_module -> return Nothing

-- | Check that a module is safe to import (according to Safe Haskell).
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an error may be thrown first.
isModuleTrusted :: GhcMonad m => Module -> m Bool
isModuleTrusted m = withSession $ \hsc_env ->
    liftIO $ hscCheckSafe hsc_env m noSrcSpan

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
moduleTrustReqs :: GhcMonad m => Module -> m (Bool, Set UnitId)
moduleTrustReqs m = withSession $ \hsc_env ->
    liftIO $ hscGetSafe hsc_env m noSrcSpan

-- | Set the monad GHCi lifts user statements into.
--
-- Checks that a type (in string form) is an instance of the
-- @GHC.GHCi.GHCiSandboxIO@ type class. Sets it to be the GHCi monad if it is,
-- throws an error otherwise.
setGHCiMonad :: GhcMonad m => String -> m ()
setGHCiMonad name = withSession $ \hsc_env -> do
    ty <- liftIO $ hscIsGHCiMonad hsc_env name
    modifySession $ \s ->
        let ic = (hsc_IC s) { ic_monad = ty }
        in s { hsc_IC = ic }

-- | Get the monad GHCi lifts user statements into.
getGHCiMonad :: GhcMonad m => m Name
getGHCiMonad = fmap (ic_monad . hsc_IC) getSession

getHistorySpan :: GhcMonad m => History -> m SrcSpan
getHistorySpan h = withSession $ \hsc_env ->
    return $ GHC.Runtime.Eval.getHistorySpan hsc_env h

obtainTermFromVal :: GhcMonad m => Int ->  Bool -> Type -> a -> m Term
obtainTermFromVal bound force ty a = withSession $ \hsc_env ->
    liftIO $ GHC.Runtime.Eval.obtainTermFromVal hsc_env bound force ty a

obtainTermFromId :: GhcMonad m => Int -> Bool -> Id -> m Term
obtainTermFromId bound force id = withSession $ \hsc_env ->
    liftIO $ GHC.Runtime.Eval.obtainTermFromId hsc_env bound force id


-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupName name =
     withSession $ \hsc_env ->
       liftIO $ hscTcRcLookupName hsc_env name

-- -----------------------------------------------------------------------------
-- Pure API

-- | A pure interface to the module parser.
--
parser :: String         -- ^ Haskell module source text (full Unicode is supported)
       -> DynFlags       -- ^ the flags
       -> FilePath       -- ^ the filename (for source locations)
       -> (WarningMessages, Either ErrorMessages (Located HsModule))

parser str dflags filename =
   let
       loc  = mkRealSrcLoc (mkFastString filename) 1 1
       buf  = stringToStringBuffer str
   in
   case unP Parser.parseModule (mkPState dflags buf loc) of

     PFailed pst ->
         let (warns,errs) = getMessages pst dflags in
         (warns, Left errs)

     POk pst rdr_module ->
         let (warns,_) = getMessages pst dflags in
         (warns, Right rdr_module)

-- -----------------------------------------------------------------------------
-- | Find the package environment (if one exists)
--
-- We interpret the package environment as a set of package flags; to be
-- specific, if we find a package environment file like
--
-- > clear-package-db
-- > global-package-db
-- > package-db blah/package.conf.d
-- > package-id id1
-- > package-id id2
--
-- we interpret this as
--
-- > [ -hide-all-packages
-- > , -clear-package-db
-- > , -global-package-db
-- > , -package-db blah/package.conf.d
-- > , -package-id id1
-- > , -package-id id2
-- > ]
--
-- There's also an older syntax alias for package-id, which is just an
-- unadorned package id
--
-- > id1
-- > id2
--
interpretPackageEnv :: DynFlags -> IO DynFlags
interpretPackageEnv dflags = do
    mPkgEnv <- runMaybeT $ msum $ [
                   getCmdLineArg >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , cmdLineError env
                     ]
                 , getEnvVar >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , envError     env
                     ]
                 , notIfHideAllPackages >> msum [
                       findLocalEnvFile >>= probeEnvFile
                     , probeEnvName defaultEnvName
                     ]
                 ]
    case mPkgEnv of
      Nothing ->
        -- No environment found. Leave DynFlags unchanged.
        return dflags
      Just "-" -> do
        -- Explicitly disabled environment file. Leave DynFlags unchanged.
        return dflags
      Just envfile -> do
        content <- readFile envfile
        compilationProgressMsg dflags ("Loaded package environment from " ++ envfile)
        let (_, dflags') = runCmdLine (runEwM (setFlagsFromEnvFile envfile content)) dflags

        return dflags'
  where
    -- Loading environments (by name or by location)

    platformArchOs = platformMini (targetPlatform dflags)

    namedEnvPath :: String -> MaybeT IO FilePath
    namedEnvPath name = do
     appdir <- versionedAppDir (programName dflags) platformArchOs
     return $ appdir </> "environments" </> name

    probeEnvName :: String -> MaybeT IO FilePath
    probeEnvName name = probeEnvFile =<< namedEnvPath name

    probeEnvFile :: FilePath -> MaybeT IO FilePath
    probeEnvFile path = do
      guard =<< liftMaybeT (doesFileExist path)
      return path

    probeNullEnv :: FilePath -> MaybeT IO FilePath
    probeNullEnv "-" = return "-"
    probeNullEnv _   = mzero

    -- Various ways to define which environment to use

    getCmdLineArg :: MaybeT IO String
    getCmdLineArg = MaybeT $ return $ packageEnv dflags

    getEnvVar :: MaybeT IO String
    getEnvVar = do
      mvar <- liftMaybeT $ MC.try $ getEnv "GHC_ENVIRONMENT"
      case mvar of
        Right var -> return var
        Left err  -> if isDoesNotExistError err then mzero
                                                else liftMaybeT $ throwIO err

    notIfHideAllPackages :: MaybeT IO ()
    notIfHideAllPackages =
      guard (not (gopt Opt_HideAllPackages dflags))

    defaultEnvName :: String
    defaultEnvName = "default"

    -- e.g. .ghc.environment.x86_64-linux-7.6.3
    localEnvFileName :: FilePath
    localEnvFileName = ".ghc.environment" <.> versionedFilePath platformArchOs

    -- Search for an env file, starting in the current dir and looking upwards.
    -- Fail if we get to the users home dir or the filesystem root. That is,
    -- we don't look for an env file in the user's home dir. The user-wide
    -- env lives in ghc's versionedAppDir/environments/default
    findLocalEnvFile :: MaybeT IO FilePath
    findLocalEnvFile = do
        curdir  <- liftMaybeT getCurrentDirectory
        homedir <- tryMaybeT getHomeDirectory
        let probe dir | isDrive dir || dir == homedir
                      = mzero
            probe dir = do
              let file = dir </> localEnvFileName
              exists <- liftMaybeT (doesFileExist file)
              if exists
                then return file
                else probe (takeDirectory dir)
        probe curdir

    -- Error reporting

    cmdLineError :: String -> MaybeT IO a
    cmdLineError env = liftMaybeT . throwGhcExceptionIO . CmdLineError $
      "Package environment " ++ show env ++ " not found"

    envError :: String -> MaybeT IO a
    envError env = liftMaybeT . throwGhcExceptionIO . CmdLineError $
         "Package environment "
      ++ show env
      ++ " (specified in GHC_ENVIRONMENT) not found"
