{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Linker
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- GHCJS linker, collects dependencies from the object files (.js_o, js_p_o),
-- which contain linkable units with dependency information
--
-- FIXME: Jeff (2022,03): Finish module description. Specifically:
-- 1. What are the important modules this module uses
-- 2. Who is the consumer for this module (hint: DynamicLinking)
-- 3. What features are missing due to the implementation in this module? For
-- example, Are we blocked from linking foreign imports due to some code in this
-- module?
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Linker where

import           GHC.StgToJS.Linker.Types

import           GHC.JS.Syntax
import           GHC.JS.Make

import qualified GHC.SysTools.Ar
import           GHC.StgToJS.Object
import           GHC.StgToJS.Rts.Types
import           GHC.StgToJS.Types hiding (LinkableUnit)

import qualified GHC.SysTools.Ar          as Ar
import           GHC.Utils.Encoding
import           GHC.Utils.Panic
import           GHC.Unit.Module ( UnitId
                                 , Module
                                 , mkModuleName, wiredInUnitIds
                                 , moduleNameString
                                 , primUnitId
                                 )
import           GHC.Data.ShortText       (ShortText)
import qualified GHC.Data.ShortText       as T

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception        (evaluate)
import           Control.Monad

import           Data.Array
import           Data.Binary
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Function            (on)
import           Data.Int
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import           Data.IORef
import           Data.List
  (partition, nub, foldl', intercalate, group, sort, groupBy, isSuffixOf, find)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import           GHC.Generics

import           System.FilePath (splitPath, (<.>), (</>), dropExtension)
import           System.IO
import           System.Directory ( createDirectoryIfMissing
                                  , canonicalizePath , doesFileExist
                                  , getCurrentDirectory, copyFile)

import Prelude

-- number of bytes linked per module
type LinkerStats  = Map Module Int64

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: BL.ByteString -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats   -- ^ statistics about generated code
  , linkOutMetaSize :: Int64         -- ^ size of packed metadata in generated code
  , linkForeignRefs :: [ForeignJSRef]  -- ^ foreign code references in compiled haskell code
  , linkLibRTS      :: [FilePath]    -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]    -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]    -- ^ library code to load from archives after RTS
  , linkBase        :: Base          -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

instance Binary LinkResult


newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

-- | link and write result to disk (jsexe directory)
link :: DynFlags
     -> GhcjsEnv
     -> JSLinkConfig
     -> FilePath                   -- ^ output file/directory
     -> [FilePath]                 -- ^ include path for home package
     -> [UnitId]          -- ^ packages to link
     -> [LinkedObj]                -- ^ the object files we're linking
     -> [FilePath]                 -- ^ extra js files to include
     -> (ExportedFun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
     -> Set ExportedFun                    -- ^ extra symbols to link in
     -> IO ()
link dflags env settings out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | gsNoJSExecutables settings = return ()
  | otherwise = do
      LinkResult lo lstats lmetasize lfrefs llW lla llarch lbase <-
        link' dflags env settings out include pkgs objFiles jsFiles
              isRootFun extraStaticDeps
      let genBase = isJust (gsGenBase settings)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      BL.writeFile (out </> "out" <.> jsExt) lo
      unless (gsOnlyOut settings) $ do
        let frefsFile   = if genBase then "out.base.frefs" else "out.frefs"
            -- FIXME: Jeff (2022,03): GHCJS used Aeson to encode Foreign
            -- references as StaticDeps to a Bytestring and then write these out
            -- to a tmp file for linking. We do not have access to Aeson so
            -- we'll need to find an alternative coding strategy to write these
            -- out. See the commented instance for FromJSON StaticDeps below.
            -- - this line called out to the FromJSon Instance
            -- |
            -- v
            -- jsonFrefs  = Aeson.encode lfrefs

        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")
        unless (gsNoStats settings) $ do
          let statsFile = if genBase then "out.base.stats" else "out.stats"
          writeFile (out </> statsFile) (linkerStats lmetasize lstats)
        unless (gsNoRts settings) $ do
          withRts <- mapM (tryReadShimFile dflags) llW
          BL.writeFile (out </> "rts.js")
            (rtsDeclsText
             <> BL.fromChunks withRts
             <> rtsText' dflags (dfCgSettings dflags))
        lla'    <- mapM (tryReadShimFile dflags) lla
        llarch' <- mapM (readShimsArchive dflags) llarch
        BL.writeFile (out </> "lib" <.> jsExt)
                     (BL.fromChunks $ llarch' ++ lla')
        if genBase
          then generateBase out lbase
          else when (not (gsOnlyOut settings) &&
                     not (gsNoRts settings) &&
                     not (usingBase settings)) $ do
                 combineFiles dflags out
                 writeHtml dflags out
                 writeRunMain dflags out
                 writeRunner settings dflags out
                 writeWebAppManifest dflags out
                 writeExterns out

-- | link in memory
link' :: DynFlags
      -> GhcjsEnv
      -> JSLinkConfig
      -> String                     -- ^ target (for progress message)
      -> [FilePath]                 -- ^ include path for home package
      -> [UnitId]          -- ^ packages to link
      -> [LinkedObj]                -- ^ the object files we're linking
      -> [FilePath]                 -- ^ extra js files to include
      -> (ExportedFun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
      -> Set ExportedFun                    -- ^ extra symbols to link in
      -> IO LinkResult
link' dflags env settings target _include pkgs objFiles jsFiles isRootFun extraStaticDeps = do
      (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles
      let rootSelector | Just baseMod <- gsGenBase settings =
                           \(ExportedFun _p m _s) -> m == T.pack baseMod
                       | otherwise = isRootFun
          roots = S.fromList . filter rootSelector $
            concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
          rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
          objPkgs = map toPackageKey $ nub (map fst $ M.keys objDepsMap)
      compilationProgressMsg dflags $
        case gsGenBase settings of
          Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ baseMod ++ ")"
          _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"
      base <- case gsUseBase settings of
        NoBase        -> return emptyBase
        BaseFile file -> loadBase file
        BaseState b   -> return b
      (rdPkgs, rds) <- rtsDeps dflags pkgs
      -- c   <- newMVar M.empty
      let rtsPkgs     =  map stringToUnitId
                             ["@rts", "@rts_" ++ buildTag dflags]
          pkgs' :: [UnitId]
          pkgs'       = nub (rtsPkgs ++ rdPkgs ++ reverse objPkgs ++ reverse pkgs)
          pkgs''      = filter (not . isAlreadyLinked base) pkgs'
          -- pkgLibPaths = mkPkgLibPaths pkgs'
          -- getPkgLibPaths :: UnitId -> ([FilePath],[String])
          -- getPkgLibPaths k = fromMaybe ([],[]) (lookup k pkgLibPaths)
      (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env =<<
          getPackageArchives dflags (map snd $ mkPkgLibPaths pkgs')
      pkgArchs <- getPackageArchives dflags (map snd $ mkPkgLibPaths pkgs'')
      (allDeps, code) <-
        collectDeps dflags
                    (objDepsMap `M.union` archsDepsMap)
                    (pkgs' ++ [thisUnitId dflags])
                    (baseUnits base)
                    (roots `S.union` rds `S.union` extraStaticDeps)
                    (archsRequiredUnits ++ objRequiredUnits)
      let (outJs, metaSize, compactorState, stats) =
             renderLinker settings dflags (baseCompactorState base) rds code
          base'  = Base compactorState (nub $ basePkgs base ++ map mkPackage pkgs'')
                         (allDeps `S.union` baseUnits base)
      (alreadyLinkedBefore, alreadyLinkedAfter) <- getShims dflags [] (filter (isAlreadyLinked base) pkgs')
      (shimsBefore, shimsAfter) <- getShims dflags jsFiles pkgs''
      return $ LinkResult outJs stats metaSize
                 (concatMap (\(_,_,_,_,_,_,r) -> r) code)
                 (filter (`notElem` alreadyLinkedBefore) shimsBefore)
                 (filter (`notElem` alreadyLinkedAfter)  shimsAfter)
                 pkgArchs base'
  where
    isAlreadyLinked :: Base -> UnitId -> Bool
    isAlreadyLinked b pkg = mkPackage pkg `elem` basePkgs b

    mkPkgLibPaths :: [UnitId] -> [(UnitId, ([FilePath],[String]))]
    mkPkgLibPaths
      = map (\k -> ( k
                   , (getInstalledPackageLibDirs dflags k
                     , getInstalledPackageHsLibs dflags k)
                   ))

renderLinker :: JSLinkConfig
             -> DynFlags
             -> CompactorState
             -> Set ExportedFun
             -> [(Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef])] -- ^ linked code per module
             -> (BL.ByteString, Int64, CompactorState, LinkerStats)
renderLinker settings dflags renamerState rtsDeps code =
  let (renamerState', compacted, meta) = Compactor.compact settings dflags renamerState (map funSymbol $ S.toList rtsDeps) (map (\(_,_,s,_,ci,si,_) -> (s,ci,si)) code)
      pe = (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = unlines . filter (not . T.null) $ map (\(_,_,_,rs,_,_,_) -> rs) code
      mkStat (p,m,_,_,_,_,_) b = ((p,m), BL.length b)
  in ( mconcat rendered <> renderedMeta <> renderedExports
     , BL.length renderedMeta
     , renamerState'
     , M.fromList $ zipWith mkStat code rendered
     )

linkerStats :: Int64         -- ^ code size of packed metadata
            -> LinkerStats   -- ^ code size per module
            -> ShortText
linkerStats meta s =
  intercalate "\n\n" [packageStats, moduleStats, metaStats] <> "\n\n"
  where
    ps = M.fromListWith (+) . map (\((p,_),s) -> (p,s)) . M.toList $ s
    pad n t = let l = length t
              in  if l < n then t <> replicate (n-l) " " else t
    pkgMods = groupBy ((==) `on` fst . fst) (M.toList s)
    showMod ((_,m),s) = pad 40 ("    " <> show m <> ":") <> show s
    packageStats = "code size summary per package:\n\n"
                   <> map (\(p,s) -> pad 25 (showPkg p <> ":") <> show s) $ M.toList ps
    moduleStats = "code size per module:\n\n" <> unlines
      (map (\xs@(((p,_),_):_) -> showPkg p <> "\n" <> unlines (map showMod xs)) pkgMods)
    metaStats = "packed metadata: " <> T.pack (show meta)

rtsText' :: DynFlags -> CgSettings -> ShortText
rtsText' = rtsText
{- prerender RTS for faster linking (FIXME this results in a build error, why?)
rtsText' debug = if debug
                   then TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ True))
                   else TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ False))
-}

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` ("/\\"::String))) . splitPath

getPackageArchives :: StgToJSConfig -> [([FilePath],[String])] -> IO [FilePath]
getPackageArchives cfg pkgs =
  filterM doesFileExist [ p </> "lib" ++ l ++ profSuff <.> "a"
                        | (paths, libs) <- pkgs, p <- paths, l <- libs ]
  where
    -- XXX the profiling library name is probably wrong now
    profSuff | csProf cfg = "_p"
             | otherwise  = ""

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: [FilePath] -> [UnitId] -> IO ([FilePath], [FilePath])
getShims = panic "Panic from getShims: Shims not implemented! no to shims!"
-- getShims dflags extraFiles pkgDeps = do
--   (w,a) <- collectShims (getLibDir dflags </> "shims")
--                         (map (convertPkg dflags) pkgDeps)
--   extraFiles' <- mapM canonicalizePath extraFiles
--   return (w, a++extraFiles')

convertPkg :: DynFlags -> UnitId -> (ShortText, Version)
convertPkg dflags p
  = case getInstalledPackageVersion dflags p of
      Just v -> (T.pack (getInstalledPackageName dflags p), v)
      -- special or wired-in
      Nothing -> (T.pack (installedUnitIdString p), Version [])

{- | convenience: combine rts.js, lib.js, out.js to all.js that can be run
     directly with node.js or SpiderMonkey jsshell
 -}
combineFiles :: DynFlags -> FilePath -> IO ()
combineFiles dflags fp = do
  files   <- mapM (B.readFile.(fp</>)) ["rts.js", "lib.js", "out.js"]
  runMain <- if   gopt Opt_NoHsMain dflags
             then pure mempty
             else B.readFile (getLibDir dflags </> "runmain.js")
  writeBinaryFile (fp</>"all.js") (mconcat (files ++ [runMain]))

-- | write the index.html file that loads the program if it does not exit
writeHtml :: DynFlags -> FilePath -> IO ()
writeHtml df out = do
  e <- doesFileExist htmlFile
  unless e $
    B.readFile (getLibDir df </>"template.html") >>= B.writeFile htmlFile
  where
    htmlFile = out </> "index.html"

-- | write the runmain.js file that will be run with defer so that it runs after index.html is loaded
writeRunMain :: DynFlags -> FilePath -> IO ()
writeRunMain df out = do
  e <- doesFileExist runMainFile
  unless e $
    B.readFile (getLibDir df </> "runmain.js") >>= B.writeFile runMainFile
  where
    runMainFile = out </> "runmain.js"

writeRunner :: JSLinkConfig -> DynFlags -> FilePath -> IO ()
writeRunner settings dflags out =
  {-when (gsBuildRunner settings) $ -} do
  cd    <- getCurrentDirectory
  let runner = cd </> addExeExtension (dropExtension out)
      srcFile = out </> "all" <.> "js"
  -- nodeSettings <- readNodeSettings dflags
  let nodePgm = "node" -- XXX we don't read nodeSettings.json anymore, we should somehow know how to find node?
  if Platform.isWindows
  then do
    copyFile (topDir dflags </> "bin" </> "wrapper" <.> "exe")
             runner
    writeFile (runner <.> "options") $ unlines
                [ T.pack nodePgm -- T.pack (nodeProgram nodeSettings)
                , T.pack ("{{EXEPATH}}" </> out </> "all" <.> "js")
                ]
  else do
    src <- readBinaryFile (cd </> srcFile)
    -- let pgm = TE.encodeUtf8 (T.pack $ nodeProgram nodeSettings)
    B.writeFile runner ("#!/usr/bin/env " <> T.pack nodePgm <> "\n" <> src)
    -- FIXME: Jeff (2022,03): set the runner file as an executable in the file system
    Cabal.setFileExecutable runner

-- | write the manifest.webapp file that for firefox os
writeWebAppManifest :: DynFlags -> FilePath -> IO ()
writeWebAppManifest df out = do
  e <- doesFileExist manifestFile
  unless e $
    B.readFile (getLibDir df </> "manifest.webapp") >>= B.writeFile manifestFile
  where
    manifestFile = out </> "manifest.webapp"

rtsExterns :: ShortText
rtsExterns =
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> T.pack (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.js.externs") rtsExterns

-- | get all functions in a module
modFuns :: Deps -> [ExportedFun]
modFuns (Deps _p _m _r e _b) = M.keys e

-- | get all dependencies for a given set of roots
getDeps :: Map Module Deps  -- ^ loaded deps
        -> Set LinkableUnit -- ^ don't link these blocks
        -> Set ExportedFun  -- ^ start here
        -> [LinkableUnit]   -- ^ and also link these
        -> IO (Set LinkableUnit)
getDeps lookup base fun startlu = go' S.empty (S.fromList startlu) (S.toList fun)
  where
    go :: Set LinkableUnit
       -> Set LinkableUnit
       -> IO (Set LinkableUnit)
    go result open = case S.minView open of
      Nothing -> return result
      Just (lu@(lpkg,lmod,n), open') ->
          let key = (lpkg, lmod)
          in  case M.lookup (lpkg,lmod) lookup of
                Nothing -> error ("getDeps.go: object file not loaded for:  " ++ show key)
                Just (Deps _ _ _ _ b) ->
                  let block = b!n
                      result' = S.insert lu result
                  in go' result'
                         (addOpen result' open' $ map (lpkg,lmod,) (blockBlockDeps block))
                         (blockFunDeps block)

    go' :: Set LinkableUnit
        -> Set LinkableUnit
        -> [ExportedFun]
        -> IO (Set LinkableUnit)
    go' result open [] = go result open
    go' result open (f:fs) =
        let key = (funPackage f, funModule f)
        in  case M.lookup key lookup of
              Nothing -> error ("getDeps.go': object file not loaded for:  " ++ show key)
              Just (Deps _p _m _r e _b) ->
                 let lun :: Int
                     lun = fromMaybe (error $ "exported function not found: " ++ show f)
                                     (M.lookup f e)
                     lu  = (funPackage f, funModule f, lun)
                 in  go' result (addOpen result open [lu]) fs

    addOpen :: Set LinkableUnit -> Set LinkableUnit -> [LinkableUnit]
            -> Set LinkableUnit
    addOpen result open newUnits =
      let alreadyLinked s = S.member s result ||
                            S.member s open   ||
                            S.member s base
      in  open `S.union` S.fromList (filter (not . alreadyLinked) newUnits)

-- | collect dependencies for a set of roots
collectDeps :: DynFlags
            -> Map Module (Deps, DepsLocation)
            -> [UnitId]     -- ^ packages, code linked in this order
            -> Set LinkableUnit -- ^ do not include these
            -> Set ExportedFun -- ^ roots
            -> [LinkableUnit] -- ^ more roots
            -> IO ( Set LinkableUnit
                  , [(Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef])]
                  )
collectDeps _dflags lookup packages base roots units = do
  allDeps <- getDeps (fmap fst lookup) base roots units
  -- read ghc-prim first, since we depend on that for static initialization
  let packages' = uncurry (++) $ partition (== toUnitId primUnitId) (nub packages)
      unitsByModule :: Map Module IntSet
      unitsByModule = M.fromListWith IS.union $
                      map (\(p,m,n) -> ((p,m),IS.singleton n)) (S.toList allDeps)
      lookupByPkg :: Map Module [(Deps, DepsLocation)]
      lookupByPkg = M.fromListWith (++) (map (\((p,_m),v) -> (p,[v])) (M.toList lookup))
  ar_state <- emptyArchiveState
  code <- fmap (catMaybes . concat) . forM packages' $ \pkg ->
    mapM (uncurry $ extractDeps ar_state unitsByModule)
         (fromMaybe [] $ M.lookup (mkPackage pkg) lookupByPkg)
  return (allDeps, code)

extractDeps :: ArchiveState
            -> Map Module IntSet
            -> Deps
            -> DepsLocation
            -> IO (Maybe (Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef]))
extractDeps ar_state units deps loc =
  case M.lookup (pkg, mod) units of
    Nothing       -> return Nothing
    Just modUnits -> do
      let selector n _  = n `IS.member` modUnits || isGlobalUnit n
      x <- case loc of
        ObjectFile o  -> collectCode =<< readObjectFileKeys selector o
        ArchiveFile a -> collectCode . readObjectKeys (a ++ ':':T.unpack mod) selector =<<
                              readArObject ar_state mod a
                            --  error ("Ar.readObject: " ++ a ++ ':' : T.unpack mod))
                            -- Ar.readObject (mkModuleName $ T.unpack mod) a)
        InMemory n b  -> collectCode $
                            readObjectKeys n selector (BL.fromStrict b)
      evaluate (rnf x)
      return x
  where
    pkg           = depsPackage deps
    mod           = depsModule deps
    collectCode l = let x = ( pkg
                            , mod
                            , mconcat (map oiStat l)
                            , unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l
                            , concatMap oiFImports l)
                    in evaluate (rnf x) >> return (Just x)

readArObject :: ArchiveState -> ShortText -> FilePath -> IO BL.ByteString
readArObject ar_state mod_name ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_state)
  ar@(Ar.Archive entries) <- case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_state) (M.insert ar_file a)
      pure a
  let tag = moduleNameTag mod_name
      matchTag entry
        | Right hdr <- getHeader (BL.fromStrict $ Ar.filedata entry)
        = hdrModuleName hdr == tag
        | otherwise = False

  -- XXX this shouldn't be an exception probably    
  pure $ maybe (error $ "could not find object for module " ++ T.unpack mod_name ++ " in " ++ ar_file) (BL.fromStrict . Ar.filedata) (find matchTag entries)
  -- mapM_ (\e -> putStrLn ("found file: " ++ Ar.filename e)) entries

mkPackage :: UnitId -> Module
mkPackage pk = Package (T.pack $ installedUnitIdString pk)

toPackageKey :: Module -> UnitId
toPackageKey = stringToUnitId . T.unpack . unPackage

{- | Static dependencies are symbols that need to be linked regardless
     of whether the linked program refers to them. For example
     dependencies that the RTS uses or symbols that the user program
     refers to directly
 -}
newtype StaticDeps =
  StaticDeps { unStaticDeps :: [(ShortText, ShortText, ShortText)] -- package/module/symbol
             }

noStaticDeps :: StaticDeps
noStaticDeps = StaticDeps []

{- | The input file format for static deps is a yaml document with a
     package/module/symbol tree where symbols can be either a list or
     just a single string, for example:

     base:
       GHC.Conc.Sync:          reportError
       Control.Exception.Base: nonTermination
     ghcjs-prim:
       GHCJS.Prim:
         - JSVal
         - JSException
 -}
-- instance FromJSON StaticDeps where
--   parseJSON (Object v) = StaticDeps . concat <$> mapM (uncurry parseMod) (HM.toList v)
--     where
--       parseMod p (Object v) = concat <$> mapM (uncurry (parseSymb p)) (HM.toList v)
--       parseMod _ _          = mempty
--       parseSymb p m (String s) = pure [(p,m,s)]
--       parseSymb p m (Array v)  = mapM (parseSingleSymb p m) (V.toList v)
--       parseSymb _ _ _          = mempty
--       parseSingleSymb p m (String s) = pure (p,m,s)
--       parseSingleSymb _ _ _          = mempty
--   parseJSON _          = mempty

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: DynFlags -> [UnitId] -> IO ([UnitId], Set ExportedFun)
rtsDeps dflags pkgs = readSystemDeps dflags pkgs
                                "RTS"
                                "linking"
                                "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: DynFlags -> [UnitId] -> IO ([UnitId], Set ExportedFun)
thDeps dflags pkgs = readSystemDeps dflags pkgs
                               "Template Haskell"
                               "running Template Haskell"
                               "thdeps.yaml"

readSystemDeps :: DynFlags
               -> [UnitId]
               -> String
               -> String
               -> FilePath
               -> IO ([UnitId], Set ExportedFun)
readSystemDeps dflags pkgs depsName requiredFor file = do
  (deps_pkgs, deps_funs) <- readSystemDeps' dflags depsName requiredFor file
  pure ( filter (`S.member` linked_pkgs) deps_pkgs
       , S.filter (\fun -> funPackage fun `S.member` linked_pkgs_pkg) deps_funs
       )

  where
    linked_pkgs     = S.fromList pkgs
    linked_pkgs_pkg = S.fromList (map (Package . T.pack . installedUnitIdString) pkgs)


readSystemDeps' :: DynFlags
               -> String
               -> String
               -> FilePath
               -> IO ([UnitId], Set ExportedFun)
readSystemDeps' dflags depsName requiredFor file
  -- hardcode contents to get rid of yaml dep
  -- XXX move runTHServer to some suitable wired-in package
  | file == "thdeps.yaml" = pure ( [stringToUnitId "base"]
                                 , S.fromList $ d "base" "GHCJS.Prim.TH.Eval" ["runTHServer"])
  | file == "rtsdeps.yaml" = pure ( [stringToUnitId "base"
                                    , stringToUnitId "ghc-prim"
                                    , stringToUnitId "integer-wired-in"
                                    ]
                                  , S.fromList $ concat
                                  [ d "base" "GHC.Conc.Sync" ["reportError"]
                                  , d "base" "Control.Exception.Base" ["nonTermination"]
                                  , d "base" "GHC.Exception.Type" ["SomeException"]
                                  , d "base" "GHC.TopHandler" ["runMainIO", "topHandler"]
                                  , d "base" "GHC.Base" ["$fMonadIO"]
                                  , d "base" "GHC.Maybe" ["Nothing", "Just"]
                                  , d "base" "GHC.Ptr" ["Ptr"]
                                  , d "ghc-prim" "GHC.Types" [":", "[]"]
                                  , d "ghc-prim" "GHC.Tuple" ["(,)", "(,,)", "(,,,)", "(,,,,)", "(,,,,,)","(,,,,,,)", "(,,,,,,,)", "(,,,,,,,,)", "(,,,,,,,,,)"]
                                  , d "integer-wired-in" "GHC.Integer.Type" ["S#", "Jp#", "Jn#"]
                                  , d "ghc-prim" "GHC.Types" [ "JSVal" ]
                                  , d "base" "GHCJS.Prim" ["JSException", "$fShowJSException", "$fExceptionJSException", "resolve", "resolveIO", "toIO"]
                                  , d "base" "GHCJS.Prim.Internal" ["wouldBlock", "blockedIndefinitelyOnMVar", "blockedIndefinitelyOnSTM", "ignoreException", "setCurrentThreadResultException", "setCurrentThreadResultValue"]
                                  ]
                                  )
  | otherwise = pure ([], mempty)
  where
    d pkg m syms = map (ExportedFun (Package $ T.pack pkg) m . mkHaskellSym (stringToUnitId pkg) m) syms
    zenc  = T.pack . zEncodeString . T.unpack
    mkHaskellSym :: UnitId -> ShortText -> ShortText -> ShortText
    mkHaskellSym p m s = "h$" <> zenc (T.pack (encodeUnitId dflags p) <> ":" <> m <> "." <> s)

{-
  b  <- readBinaryFile (getLibDir dflags </> file)
  wi <- readSystemWiredIn dflags
  case Yaml.decodeEither b of
    Left err -> panic $ "could not read " ++ depsName ++
                        " dependencies from " ++ file ++ ":\n" ++ err
    Right sdeps ->
      let (StaticDeps unresolved, pkgs, funs) = staticDeps dflags wi sdeps
      in  case unresolved of
            ((p,_,_):_) ->
                  panic $ "Package `" ++ T.unpack p ++ "' is required for " ++
                          requiredFor ++ ", but was not found"
            _ ->
              -- putStrLn "system dependencies:"
              -- print (map installedUnitIdString pkgs, funs)
              return (pkgs, funs)

-}

readSystemWiredIn :: DynFlags -> IO [(ShortText, UnitId)]
readSystemWiredIn _ = pure [] -- XXX
{-
readSystemWiredIn dflags = do
  b <- B.readFile filename
  case Yaml.decodeEither b of
     Left _err -> error $ "could not read wired-in package keys from " ++ filename
     Right m  -> return . M.toList
                        . M.union ghcWiredIn -- GHC wired-in package keys override those in the file
                        . fmap stringToUnitId $ m
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
    ghcWiredIn :: Map Text UnitId
    ghcWiredIn = M.fromList $ map (\k -> (T.pack (installedUnitIdString k), k))
                                  (map toUnitId wiredInUnitIds)
                                  -}
{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

type SDep = (ShortText, ShortText, ShortText)

staticDeps :: DynFlags
           -> [(ShortText, UnitId)]    -- ^ wired-in package names / keys
           -> StaticDeps              -- ^ deps from yaml file
           -> (StaticDeps, [UnitId], Set ExportedFun)
                                      -- ^ the StaticDeps contains the symbols
                                      --   for which no package could be found
staticDeps dflags wiredin sdeps = mkDeps sdeps
  where
    zenc  = T.pack . zEncodeString . T.unpack
    mkDeps (StaticDeps ds) =
      let (u, p, r) = foldl' resolveDep ([], S.empty, S.empty) ds
      in  (StaticDeps u, S.toList (closePackageDeps dflags p), r)
    resolveDep :: ([SDep], Set UnitId, Set ExportedFun)
               -> SDep
               -> ([SDep], Set UnitId, Set ExportedFun)
    resolveDep (unresolved, pkgs, resolved) dep@(p, m, s) =
      case lookup p wiredin of
             Nothing -> ( dep : unresolved, pkgs, resolved)
             Just k  -> case lookupUnitId dflags k of
               Nothing -> error $ "Package key for wired-in dependency `" ++
                                  T.unpack p ++ "' could not be found: "  ++
                                  installedUnitIdString k
               Just conf ->
                 let k' = unitId conf
                 in  ( unresolved
                     , S.insert k' pkgs
                     , S.insert (ExportedFun (mkPackage k') m $ mkSymb k' m s)
                                resolved
                     )
    mkSymb :: UnitId -> ShortText -> ShortText -> ShortText
    mkSymb p m s  =
      "h$" <> zenc (T.pack (encodeUnitId dflags p) <> ":" <> m <> "." <> s)

closePackageDeps :: DynFlags -> Set UnitId -> Set UnitId
closePackageDeps dflags pkgs
  | S.size pkgs == S.size pkgs' = pkgs
  | otherwise                   = closePackageDeps dflags pkgs'
  where
    pkgs' = pkgs `S.union` S.fromList (concatMap deps $ S.toList pkgs)
    notFound = error "closePackageDeps: package not found"
    deps :: UnitId -> [UnitId]
    deps =
--           map (Packages.resolveInstalledPackageId dflags)
           depends
         . fromMaybe notFound
         . lookupUnitId dflags

-- read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map Module (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = prepareLoadedDeps <$> mapM readDepsFile' objs

loadArchiveDeps :: GhcjsEnv
                -> [FilePath]
                -> IO ( Map Module (Deps, DepsLocation)
                      , [LinkableUnit]
                      )
loadArchiveDeps env archives = modifyMVar (linkerArchiveDeps env) $ \m ->
  case M.lookup archives' m of
    Just r  -> return (m, r)
    Nothing -> loadArchiveDeps' archives >>= \r -> return (M.insert archives' r m, r)
  where
     archives' = S.fromList archives

loadArchiveDeps' :: [FilePath]
                 -> IO ( Map Module (Deps, DepsLocation)
                       , [LinkableUnit]
                       )
loadArchiveDeps' archives = do
  archDeps <- forM archives $ \file -> do
    ar@(Ar.Archive entries) <- Ar.loadAr file
    pure (mapMaybe (readEntry file) entries)
  return (prepareLoadedDeps $ concat archDeps)
    where
      readEntry :: FilePath -> Ar.ArchiveEntry -> Maybe (Deps, DepsLocation)
      readEntry ar_file ar_entry
        | isObjFile (Ar.filename ar_entry) =
            fmap (,ArchiveFile ar_file)
                 (readDepsMaybe (ar_file ++ ':':Ar.filename ar_entry) (BL.fromStrict $ Ar.filedata ar_entry))
        | otherwise = Nothing


isObjFile :: FilePath -> Bool
isObjFile file = ".o" `isSuffixOf` file || -- vanilla
                 "_o" `isSuffixOf` file    -- some "Way", like .p_o

prepareLoadedDeps :: [(Deps, DepsLocation)]
                  -> ( Map Module (Deps, DepsLocation)
                     , [LinkableUnit]
                     )
prepareLoadedDeps deps =
  let req     = concatMap (requiredUnits . fst) deps
      depsMap = M.fromList $ map (\d -> ((depsPackage (fst d)
                                         ,depsModule (fst d)), d))
                                 deps
  in  (depsMap, req)

requiredUnits :: Deps -> [LinkableUnit]
requiredUnits d = map (depsPackage d, depsModule d,)
                      (IS.toList $ depsRequired d)

-- read dependencies from an object that might have already been into memory
-- pulls in all Deps from an archive
readDepsFile' :: LinkedObj -> IO (Deps, DepsLocation)
readDepsFile' (ObjLoaded name bs) = pure . (,InMemory name bs) $
                                    readDeps name (BL.fromStrict bs)
readDepsFile' (ObjFile file)      =
  (,ObjectFile file) <$> readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (renderBase b)

