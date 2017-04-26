module Rules.Library (buildPackageLibrary, buildPackageGhciLibrary) where

import Data.Char
import qualified System.Directory as IO

import Base
import Context
import Expression
import Flavour
import GHC
import Oracles.ModuleFiles
import Oracles.PackageData
import Settings
import Settings.Path
import Target
import UserSettings
import Util

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context@Context {..} = do
    let path       = buildPath context
        libPrefix  = path -/- "libHS" ++ pkgNameString package
    -- TODO: handle dynamic libraries
    matchVersionedFilePath libPrefix (waySuffix way <.> "a") ?> \a -> do
        removeFile a
        hsObjs   <- hsObjects    context
        noHsObjs <- nonHsObjects context

        -- This will create split objects if required (we don't track them
        -- explicitly as this would needlessly bloat the Shake database).
        need $ noHsObjs ++ hsObjs

        split <- interpretInContext context $ splitObjects flavour
        let getSplitObjs = concatForM hsObjs $ \obj -> do
                let dir = dropExtension obj ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents dir
                return . map (dir -/-) $ filter (not . all (== '.')) contents

        objs <- (noHsObjs ++) <$> if split then getSplitObjs else return hsObjs

        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        if isLib0 then build $ Target context Ar []   [a] -- TODO: Scan for dlls
                  else build $ Target context Ar objs [a]

        synopsis <- interpretInContext context $ getPkgData Synopsis
        unless isLib0 . putSuccess $ renderLibrary
            (quote (pkgNameString package) ++ " (" ++ show stage ++ ", way "
            ++ show way ++ ").") a (dropWhileEnd isPunctuation synopsis)

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    let libPrefix = buildPath context -/- "HS" ++ pkgNameString package
    matchVersionedFilePath libPrefix (waySuffix way <.> "o") ?> \obj -> do
        objs <- allObjects context
        need objs
        build $ Target context Ld objs [obj]

allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    let path = buildPath context
    cObjs   <- cObjects context
    cmmObjs <- map (objectPath context) <$> pkgDataList (CmmSrcs path)
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

cObjects :: Context -> Action [FilePath]
cObjects context = do
    objs <- map (objectPath context) <$> pkgDataList (CSrcs $ buildPath context)
    return $ if way context == threaded
        then objs
        else filter ((`notElem` ["Evac_thr", "Scav_thr"]) . takeBaseName) objs

extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == integerGmp = do
        need [gmpLibraryH]
        map unifyPath <$> getDirectoryFiles "" [gmpObjects -/- "*.o"]
    | otherwise         = return []
