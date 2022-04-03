{-# LANGUAGE CPP                      #-}
{-# LANGUAGE TupleSections            #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.DynamicLinking
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Various utilities for building and loading dynamic libraries, to make
-- Template Haskell work in GHCJS
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.DynamicLinking
  ( ghcjsLink
  , ghcjsDoLink
  -- , isGhcjsPrimPackage
  -- , ghcjsPrimPackage
  ) where

import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import GHC.StgToJS.Linker.Variants

import GHC.Linker.Types
import GHC.Linker.Static.Utils

import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Exception
import GHC.Unit.Module
import GHC.Utils.Error
import GHC.Driver.Session
import GHC.Driver.Phases
import GHC.Driver.Pipeline hiding ( linkingNeeded )

import GHC.Types.Unique.DFM
import GHC.Types.Basic
import qualified GHC.SysTools as SysTools

import GHC.Unit.Home.ModInfo
import GHC.Unit.Info
import GHC.Unit.Env

import GHC.Platform
import Prelude

import           Control.Monad

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import           Data.Either
import           Data.List ( nub )
import qualified GHC.Data.ShortText as T

import           System.Directory
import           System.FilePath
import qualified GHC.Data.Maybe as Maybe
import GHC.Platform.Ways
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Driver.Env.Types
import GHC.StgToJS.Types
import GHC.Utils.TmpFs (TmpFs)

-------------------------------------------------------------------------------------------
-- Link libraries

ghcjsLink :: GhcjsEnv
          -> JSLinkConfig
          -> StgToJSConfig
          -> [FilePath] -- ^ extra JS files
          -> Bool       -- ^ build JavaScript?
          -> GhcLink    -- ^ what to link
          -> DynFlags
          -> Logger
          -> TmpFs
          -> UnitEnv
          -> Bool
          -> HomePackageTable
          -> IO SuccessFlag
ghcjsLink env lc_cfg cfg extraJs buildJs ghcLink dflags logger tmp_fs unit_env batch_attempt_linking pt
  | ghcLink == LinkInMemory || ghcLink == NoLink =
      return Succeeded
  | ghcLink == LinkStaticLib || ghcLink == LinkDynLib =
      if buildJs && Maybe.isJust (lcLinkJsLib lc_cfg)
         then ghcjsLinkJsLib lc_cfg extraJs dflags logger pt
         else return Succeeded
  | otherwise = do
      when (buildJs && Maybe.isJust (lcLinkJsLib lc_cfg))
        (void $ ghcjsLinkJsLib lc_cfg extraJs dflags logger pt) -- FIXME Jeff: (2022,04): use return value and remove void
      link' env lc_cfg cfg extraJs buildJs dflags logger tmp_fs unit_env batch_attempt_linking pt

ghcjsLinkJsLib :: JSLinkConfig
               -> [FilePath] -- ^ extra JS files
               -> DynFlags
               -> Logger
               -> HomePackageTable
               -> IO SuccessFlag
ghcjsLinkJsLib settings jsFiles dflags _logger hpt
  | Just jsLib <- lcLinkJsLib settings = do
      let profSuff | WayProf `elem` ways dflags = "_p"
                   | otherwise                  = ""
          libFileName    = ("lib" ++ jsLib ++ profSuff) <.> "js_a"
          inOutputDir file =
            maybe file
                  (</>file)
                  (lcJsLibOutputDir settings `mplus` objectDir dflags)
          outputFile     = inOutputDir libFileName
          jsFiles' = nub (lcJsLibSrcs settings ++ jsFiles)
          meta    = Meta (opt_P dflags)
      jsEntries <- forM jsFiles' $ \file ->
        (JsSource file,) . B.fromStrict <$> BS.readFile file
      objEntries <- forM (eltsUDFM hpt) $ \hmi -> do
        let mt    = T.pack . moduleNameString . moduleName . mi_module . hm_iface $ hmi
            files = maybe [] (\l -> [ o | DotO o <- linkableUnlinked l]) (hm_linkable hmi)
        -- fixme archive does not handle multiple files for a module yet
        forM files (fmap ((Object mt,) . B.fromStrict) . BS.readFile)
      B.writeFile outputFile (buildArchive meta (concat objEntries ++ jsEntries))
      -- we don't use shared js_so libraries ourselves, but Cabal expects that we
      -- generate one when building with --dynamic-too. Just write an empty file
      when (gopt Opt_BuildDynamicToo dflags || WayDyn `elem` ways dflags) $ do
        let sharedLibFileName =
              "lib" ++ jsLib ++ "-ghcjs" ++ getCompilerVersion ++ profSuff <.> "js_so"
            sharedOutputFile = inOutputDir sharedLibFileName
        -- keep strip happy
        BS.writeFile sharedOutputFile =<< BS.readFile (topDir dflags </> "empty.o")
      return Succeeded
  | otherwise =
      return Succeeded

ghcjsLinkJsBinary :: GhcjsEnv
                  -> JSLinkConfig
                  -> StgToJSConfig
                  -> [FilePath]
                  -> Logger
                  -> TmpFs
                  -> HscEnv
                  -> UnitEnv
                  -> [FilePath]
                  -> [UnitId]
                  -> IO ()
ghcjsLinkJsBinary env lc_cfg cfg jsFiles _logger _tmpfs hsc_env _unit_env objs dep_pkgs =
  void $ variantLink gen2Variant hsc_env env lc_cfg cfg exe mempty dep_pkgs objs' jsFiles isRoot mempty
    where
      objs'    = map ObjFile objs
      isRoot _ = True
      exe      = jsExeFileName dflags
      -- packageLibPaths :: UnitId -> [FilePath]
      -- packageLibPaths = maybe [] libraryDirs . lookupInstalledPackage dflags

{-
isGhcjsPrimPackage :: DynFlags -> UnitId -> Bool
isGhcjsPrimPackage dflags pkgKey
  =  getInstalledPackageName dflags pkgKey == "ghcjs-prim" ||
     (pkgKey == thisUnitId dflags &&
      elem "-DBOOTING_PACKAGE=ghcjs-prim" (opt_P dflags))

ghcjsPrimPackage :: DynFlags -> IO UnitId
ghcjsPrimPackage dflags = do
  keys <- BS.readFile filename
  case Yaml.decodeEither keys of
    Left _err -> error $ "could not read wired-in package keys from " ++ filename
    Right m -> case M.lookup "ghcjs-prim" m of
      Nothing -> error "Package `ghcjs-prim' is required to link executables"
      Just k -> return (stringToPackageKey k)
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
-}

link' :: GhcjsEnv
      -> JSLinkConfig
      -> StgToJSConfig
      -> [FilePath]              -- extra js files
      -> Bool                    -- building JavaScript
      -> DynFlags                -- dynamic flags
      -> Logger                  -- Logger
      -> TmpFs                   -- tmp file system
      -> UnitEnv                 -- Unit Environment
      -> Bool                    -- attempt linking in batch mode?
      -> HomePackageTable        -- what to link
      -> IO SuccessFlag

link' env lc_cfg cfg extraJs buildJs dflags logger tmp_fs unit_env batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

            home_mod_infos = eltsUDFM hpt

            -- the packages we depend on
            pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos
        debugTraceMsg logger 3 (text "link: hmi ..." $$ vcat (map (ppr . mi_module . hm_iface) home_mod_infos))
        debugTraceMsg logger 3 (text "link: pkgdeps ..." $$ vcat (map ppr pkg_deps))
        debugTraceMsg logger 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then
          do debugTraceMsg logger 3 (text "link(batch): linking omitted (-c flag given).")
             return Succeeded
          else
          do
            let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
                obj_files             = concatMap getOfiles linkables
                exe_file              = exeFileName (targetPlatform dflags) staticLink (outputFile_ dflags)

            linking_needed <- linkingNeeded logger dflags staticLink unit_env linkables pkg_deps

            if not (gopt Opt_ForceRecomp dflags) && not linking_needed
              then
              do
                debugTraceMsg logger 2 (text exe_file <+> ptext (sLit "is up to date, linking not required."))
                return Succeeded
              else
              do
                unless buildJs $ compilationProgressMsg logger (text $ "ghcjs Linking " ++ exe_file ++ " ...")
                -- Don't showPass in Batch mode; doLink will do that for us.

                -- FIXME: Jeff (2022,03): this let expression relies on all
                -- these functions implementing the same interface. Which leads
                -- to a lot of unused parameters. This is bad! We should be
                -- employing the principle of least priviledge with these
                -- functions. Untangle this later!
                let link = case ghcLink dflags of
                      LinkBinary    -> if buildJs
                                       then ghcjsLinkJsBinary env lc_cfg cfg extraJs logger tmp_fs hsc_env unit_env
                                       else linkBinary False                         logger tmp_fs (hsc_dflags hsc_env) unit_env
                      LinkStaticLib -> linkStaticLib   logger tmp_fs hsc_env unit_env
                      LinkDynLib    -> linkDynLibCheck logger tmp_fs hsc_env unit_env
                      other         -> panicBadLink other

                _ <- link obj_files pkg_deps
                debugTraceMsg logger 3 (text "link: done")

                -- linkBinary only returns if it succeeds
                return Succeeded

   | otherwise
   = do debugTraceMsg logger 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: Logger -> DynFlags -> Bool -> UnitEnv -> [Linkable] -> [UnitId] -> IO Bool
linkingNeeded logger dflags staticLink unit_env linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName (targetPlatform dflags) staticLink (outputFile_ dflags)
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = partitionEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

          -- next, check libraries. XXX this only checks Haskell libraries,
          -- not extra_libraries or -l things from the command line.
          let pkg_hslibs  = [ (libraryDirs c, lib)
                            | Just c <- map (lookupInstalledPackage dflags) pkg_deps
                            , lib <- packageHsLibs dflags c
                            ]

          pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
          if any isNothing pkg_libfiles
            then return True
            else
            do
              e_lib_times <- mapM (tryIO . getModificationUTCTime) (catMaybes pkg_libfiles)
              let (lib_errs,lib_times) = partitionEithers e_lib_times
              if not (null lib_errs) || any (t <) lib_times
                then return True
                else checkLinkInfo logger dflags unit_env pkg_deps exe_file

panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

linkDynLibCheck :: Logger -> TmpFs -> HscEnv -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkDynLibCheck logger _tmpfs hsc_env _unit_env o_files dep_packages
 = do
    let dflags = hsc_dflags hsc_env
    when (haveRtsOptsFlags dflags) $
      logOutput logger (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared."
                        $$ text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

linkStaticLib ::Logger -> TmpFs -> HscEnv -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkStaticLib logger _tmpfs hsc_env _unit_env o_files dep_packages
  = -- XXX looks like this needs to be updated
{-
 = do
    when (platformOS (targetPlatform dflags) `notElem` [OSiOS, OSDarwin]) $
      throwGhcExceptionIO (ProgramError "Static archive creation only supported on Darwin/OS X/iOS")
-}
    jsLinkBinary' True logger (hsc_dflags hsc_env) o_files dep_packages

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if ghcLink dflags == LinkStaticLib
                       then "lib" ++ lib <.> "a"
                       else mkSOName (targetPlatform dflags) lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

jsLinkBinary' :: Bool -> Logger -> DynFlags -> [FilePath] -> [UnitId] -> IO ()
jsLinkBinary' staticLink logger dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        mySettings = settings     dflags
        verbFlags = getVerbFlags  dflags
        output_file = outputFile_ dflags
        output_fn = exeFileName platform staticLink output_file

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if gopt Opt_RPath dflags
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if platformOS platform == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           gopt Opt_RPath dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ("-L" ++ l) : ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = ["-Wl,-dead_strip" | osSubsectionsViaSymbols (platformOS platform)]
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
        debug_opts | WayDebug `elem` ways dflags = [



                         ]
                   | otherwise            = []

        thread_opts | WayThreaded `elem` ways dflags = [



                        ]
                    | otherwise               = []

    rc_objs <- maybeCreateManifest dflags output_fn

    -- FIXME: Jeff (2022,04): linkBinary' is only ever called with staticLink ==
    -- True. However, if it were False I'm unsure how to create the TmpFS
    -- parameter required by SysTools.runLink. Is this necessary for the
    -- js-backend? Fix this when we know more.

    -- let link = if staticLink
    --                then SysTools.runLibtool
    --                else SysTools.runLink
    let link = SysTools.runLibtool -- <--- fix this line in particular

    link logger dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (["-Wl,--enable-auto-import" | platformOS platform == OSMinGW32])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ ([ "-Wl,-no_compact_unwind"
                          | sLdSupportsCompactUnwind mySettings
                            && not staticLink
                            && (platformOS platform == OSDarwin)
                            && case platformArch platform of
                              ArchX86      -> True
                              ArchX86_64   -> True
                              ArchARM {}   -> True
                              ArchAArch64  -> True
                              _            -> False])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (["-Wl,-read_only_relocs,suppress"
                          | platformOS   platform == OSDarwin
                            && platformArch platform == ArchX86
                            && not staticLink
                          ])

                      ++ (["-Wl,--gc-sections"
                          | sLdIsGnuLd mySettings
                            && not (gopt Opt_WholeArchiveHsLibs dflags)
                          ])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))


ghcjsDoLink :: GhcjsEnv -> JSLinkConfig -> StgToJSConfig -> HscEnv -> Phase -> [FilePath] -> IO ()
ghcjsDoLink env lc_cfg cfg hsc_env stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase
  {-| native
  = case ghcLink dflags of
        NoLink     -> return ()
        LinkBinary -> linkBinary      dflags o_files []
        LinkDynLib -> linkDynLibCheck dflags o_files []
        other      -> panicBadLink other -}
  -- | isJust (gsLinkJsLib settings)
  -- = void $ ghcjsLinkJsLib settings o_files dflags emptyHomePackageTable
  | otherwise = do
    -- void $ ghcjsLink env settings o_files True (ghcLink dflags) dflags True emptyHomePackageTable
      let dflags = hsc_dflags hsc_env
      case ghcLink (hsc_dflags hsc_env) of
        NoLink     -> return ()
        LinkBinary ->  do
          putStrLn $ "ghcjsDoLink: " ++ show (ghcLink dflags) ++ " " ++ show o_files
          let output_fn = exeFileName (targetPlatform dflags) False (outputFile_ dflags)
          let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
          putStrLn $ "ghcjsDoLink: " ++ show extra_ld_inputs

          full_output_fn <- if isAbsolute output_fn
                            then return output_fn
                            else do d <- getCurrentDirectory
                                    return $ normalise (d </> output_fn)
          let dep_packages = case preloadUnitsInfo $ hsc_unit_env hsc_env of
                      Maybe.Succeeded us -> us
                      Maybe.Failed err   -> pprPanic "Panic in ghcjsDoLink: " (ppr err)


          let isRoot _ = True
              objs' = map ObjFile o_files
              dep_package_ids = map unitId dep_packages
              jsFiles = [] -- XXX check whether we need to sort the obj_files for js-files

          putStrLn $ "ghcjsDoLink: " ++ show (map unitIdString dep_package_ids)

          void $
            variantLink gen2Variant
              hsc_env
              env
              lc_cfg
              cfg
              (full_output_fn <.> "jsexe")  -- output file or dir
              mempty                        -- include path for home package
              dep_package_ids               -- packages to link
              objs'                         -- object we're currently linking
              jsFiles                       -- extra js files to include
              isRoot                        -- functions from the objects to use as roots
              mempty                        -- extra symbols to link in

        _other      -> panicBadLink _other


{-
ghcjsLinkJsBinary :: GhcjsEnv
                  -> GhcjsSettings
                  -> [FilePath]
                  -> DynFlags
                  -> [FilePath]
                  -> [UnitId]
                  -> IO ()
-}

{-
link :: DynFlags
     -> GhcjsEnv
     -> GhcjsSettings
     -> FilePath                   -- ^ output file/directory
     -> [FilePath]                 -- ^ include path for home package
     -> [UnitId]          -- ^ packages to link
     -> [LinkedObj]                -- ^ the object files we're linking
     -> [FilePath]                 -- ^ extra js files to include
     -> (Fun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
     -> Set Fun                    -- ^ extra symbols to link in
     -> IO ()
-}

{-

ghcjsLinkBinary :: DynFlags -> [FilePath] -> [UnitId] -> IO ()
ghcjsLinkBinary = ghcjsLinkBinary' False

ghcjsLinkBinary' :: Bool -> DynFlags -> [FilePath] -> [UnitId] -> IO ()
ghcjsLinkBinary' staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        toolSettings' = toolSettings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    -- pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    pkg_lib_paths <- getPreloadPackagesAnd dflags dep_packages
    putStrLn $ "output fn: " ++ show full_output_fn
    putStrLn $ "lib paths: " ++ show pkg_lib_paths
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l = ["-L" ++ l]

    pkg_lib_path_opts <-
      if gopt Opt_SingleLibFolder dflags
      then do
        libs <- getLibs dflags dep_packages
        tmpDir <- newTempDir dflags
        sequence_ [ copyFile lib (tmpDir </> basename)
                  | (lib, basename) <- libs]
        return [ "-L" ++ tmpDir ]
      else pure pkg_lib_path_opts

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    rc_objs <- maybeCreateManifest dflags output_fn

    let link dflags args | staticLink = SysTools.runLibtool dflags args
                         | platformOS platform == OSDarwin
                            = SysTools.runLink dflags args >> SysTools.runInjectRPaths dflags pkg_lib_paths output_fn
                         | otherwise
                            = SysTools.runLink dflags args

    link dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ libmLinkOpts
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if toolSettings_ldSupportsCompactUnwind toolSettings' &&
                             not staticLink &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86     -> True
                               ArchX86_64  -> True
                               ArchARM {}  -> True
                               ArchAArch64 -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if toolSettings_ldIsGnuLd toolSettings' &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ (if platformOS platform == OSDarwin
                          --  dead_strip_dylibs, will remove unused dylibs, and thus save
                          --  space in the load commands. The -headerpad is necessary so
                          --  that we can inject more @rpath's later for the left over
                          --  libraries during runInjectRpaths phase.
                          --
                          --  See Note [Dynamic linking on macOS].
                          then [ "-Wl,-dead_strip_dylibs", "-Wl,-headerpad,8000" ]
                          else [])
                    ))
-}
