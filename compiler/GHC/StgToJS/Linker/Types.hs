{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
--  A base bundle is used for incremental linking. it contains information about
--  the symbols that have already been linked. These symbols are not included
--  again in the incrementally linked program.
--
--  The base contains a CompactorState for consistent renaming of private names
--  and packed initialization of info tables and static closures.

-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Types where

import           GHC.JS.Syntax
import           GHC.StgToJS.Object

import           GHC.Unit.Types
import           GHC.Utils.Panic
import           GHC.Utils.Outputable hiding ((<>))

import           Control.Monad

import           Data.Array
import qualified Data.Binary          as DB
import qualified Data.Binary.Get      as DB
import qualified Data.Binary.Put      as DB
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           GHC.Data.ShortText   (ShortText)
import qualified GHC.Data.ShortText   as T
import           Data.ByteString      (ByteString)

import           Prelude

newLocals :: [Ident]
newLocals = filter (not . isJsKeyword) $
            map (TxtI . T.pack) $
            map (:[]) chars0 ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9']

renamedVars :: [Ident]
renamedVars = map (\(TxtI xs) -> TxtI ("h$$"<>xs)) newLocals

data CompactorState = CompactorState
  { csIdentSupply   :: [Ident]               -- ^ ident supply for new names
  , csNameMap       :: !(M.Map ShortText Ident) -- ^ renaming mapping for internal names
  , csEntries       :: !(M.Map ShortText Int)   -- ^ entry functions (these get listed in the metadata init array)
  , csNumEntries    :: !Int
  , csStatics       :: !(M.Map ShortText Int)   -- ^ mapping of global closure -> index in current block, for static initialisation
  , csNumStatics    :: !Int                  -- ^ number of static entries
  , csLabels        :: !(M.Map ShortText Int)   -- ^ non-Haskell JS labels
  , csNumLabels     :: !Int                  -- ^ number of labels
  , csParentEntries :: !(M.Map ShortText Int)   -- ^ entry functions we're not linking, offset where parent gets [0..n], grandparent [n+1..k] etc
  , csParentStatics :: !(M.Map ShortText Int)   -- ^ objects we're not linking in base bundle
  , csParentLabels  :: !(M.Map ShortText Int)   -- ^ non-Haskell JS labels in parent
  , csStringTable   :: !StringTable
  } deriving (Show)

data StringTable = StringTable
  { stTableIdents :: !(Array Int ShortText)
  , stOffsets     :: !(M.Map ByteString (Int, Int))        -- ^ content of the table
  , stIdents      :: !(M.Map ShortText  (Either Int Int))  -- ^ identifiers in the table
  } deriving (Show)

instance DB.Binary StringTable where
  put (StringTable tids offs idents) = do
    DB.put tids
    DB.put (M.toList offs)
    DB.put (M.toList idents)
  get = StringTable <$> DB.get
                    <*> fmap M.fromList DB.get
                    <*> fmap M.fromList DB.get

emptyStringTable :: StringTable
emptyStringTable = StringTable (listArray (0,-1) []) M.empty M.empty

entries :: Functor f
        => (M.Map ShortText Int -> f (M.Map ShortText Int))
        -> CompactorState
        -> f CompactorState
entries f cs = fmap (\x -> cs { csEntries = x }) (f $ csEntries cs)
{-# INLINE entries #-}

identSupply :: Functor f
            => ([Ident] -> f [Ident])
            -> CompactorState
            -> f CompactorState
identSupply f cs = fmap (\x -> cs { csIdentSupply = x }) (f $ csIdentSupply cs)
{-# INLINE identSupply #-}

labels :: Functor f
       => (M.Map ShortText Int -> f (M.Map ShortText Int))
       -> CompactorState
       -> f CompactorState
labels f cs = fmap (\x -> cs { csLabels = x }) (f $ csLabels cs)
{-# INLINE labels #-}

nameMap :: Functor f
        => (M.Map ShortText Ident -> f (M.Map ShortText Ident))
        -> CompactorState
        -> f CompactorState
nameMap f cs = fmap (\x -> cs { csNameMap = x }) (f $ csNameMap cs)
{-# INLINE nameMap #-}

numEntries :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numEntries f cs = fmap (\x -> cs { csNumEntries = x }) (f $ csNumEntries cs)
{-# INLINE numEntries #-}

numLabels :: Functor f
          => (Int -> f Int)
          -> CompactorState
          -> f CompactorState
numLabels f cs = fmap (\x -> cs { csNumLabels = x }) (f $ csNumLabels cs)
{-# INLINE numLabels #-}

numStatics :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numStatics f cs = fmap (\x -> cs { csNumStatics = x }) (f $ csNumStatics cs)
{-# INLINE numStatics #-}

parentEntries :: Functor f
              => (M.Map ShortText Int -> f (M.Map ShortText Int))
              -> CompactorState
              -> f CompactorState
parentEntries f cs = fmap (\x -> cs { csParentEntries = x }) (f $ csParentEntries cs)
{-# INLINE parentEntries #-}

parentLabels :: Functor f
             => (M.Map ShortText Int -> f (M.Map ShortText Int))
             -> CompactorState
             -> f CompactorState
parentLabels f cs = fmap (\x -> cs { csParentLabels = x }) (f $ csParentLabels cs)
{-# INLINE parentLabels #-}

parentStatics :: Functor f
              => (M.Map ShortText Int -> f (M.Map ShortText Int))
              -> CompactorState
              -> f CompactorState
parentStatics f cs = fmap (\x -> cs { csParentStatics = x }) (f $ csParentStatics cs)
{-# INLINE parentStatics #-}

statics :: Functor f
        => (M.Map ShortText Int -> f (M.Map ShortText Int))
        -> CompactorState
        -> f CompactorState
statics f cs = fmap (\x -> cs { csStatics = x }) (f $ csStatics cs)
{-# INLINE statics #-}

stringTable :: Functor f
            => (StringTable -> f StringTable)
            -> CompactorState
            -> f CompactorState
stringTable f cs = fmap (\x -> cs { csStringTable = x }) (f $ csStringTable cs)
{-# INLINE stringTable #-}

emptyCompactorState :: CompactorState
emptyCompactorState = CompactorState renamedVars
                                     mempty
                                     mempty
                                     0
                                     mempty
                                     0
                                     mempty
                                     0
                                     mempty
                                     mempty
                                     mempty
                                     emptyStringTable

showBase :: Base -> String
showBase b = unlines
  [ "Base:"
  , "  packages: " ++ showSDocUnsafe (ppr (basePkgs b)) -- FIXME: Jeff (2022,03): Either use the sdoc context in the StgToJS
                                                        -- config or find a better way
  , "  number of units: " ++ show (S.size $ baseUnits b)
  , "  renaming table size: " ++
    show (M.size . csNameMap . baseCompactorState $ b)
  ]

-- FIXME: Jeff (2022,03): Pick a better name than Base
-- | The Base bundle. Used for incremental linking it maintains the compactor
-- state the base packages and units.
data Base = Base { baseCompactorState :: CompactorState
                 , basePkgs           :: [Module]
                 , baseUnits          :: Set (Module, ShortText, Int)
                 }

emptyBase :: Base
emptyBase = Base emptyCompactorState [] S.empty

putBase :: Base -> PutS
putBase (Base cs packages funs) = do
  DB.putByteString "GHCJSBASE"
  DB.putLazyByteString Object.versionTag
  putCs cs
  putList DB.put packages
  putList putPkg pkgs
  putList DB.put mods
  putList putFun (S.toList funs)
  where
    pi :: Int -> PutS
    pi = DB.putWord32le . fromIntegral
    uniq :: Ord a => [a] -> [a]
    uniq  = S.toList . S.fromList
    pkgs  = uniq (map (\(x,_,_) -> x) $ S.toList funs)
    pkgsM = M.fromList (zip pkgs [(0::Int)..])
    mods  = uniq (map (\(_,x,_) -> x) $ S.toList funs)
    modsM = M.fromList (zip mods [(0::Int)..])
    putList f xs = pi (length xs) >> mapM_ f xs
    -- serialise the compactor state
    putCs (CompactorState [] _ _ _ _ _ _ _ _ _ _ _) =
      panic "putBase: putCs exhausted renamer symbol names"
    putCs (CompactorState (ns:_) nm es _ ss _ ls _ pes pss pls sts) = do
      DB.put ns
      DB.put (M.toList nm)
      DB.put (M.toList es)
      DB.put (M.toList ss)
      DB.put (M.toList ls)
      DB.put (M.toList pes)
      DB.put (M.toList pss)
      DB.put (M.toList pls)
      DB.put sts
    putPkg mod = DB.put mod
    -- fixme group things first
    putFun (p,m,s) = pi (pkgsM M.! p) >> pi (modsM M.! m) >> DB.put s

getBase :: FilePath -> DB.Get Base
getBase file = getBase'
  where
    gi :: DB.Get Int
    gi = fromIntegral <$> DB.getWord32le
    getList f = DB.getWord32le >>= \n -> replicateM (fromIntegral n) f
    getFun ps ms = (,,) <$> ((ps!) <$> gi) <*> ((ms!) <$> gi) <*> DB.get
    la xs = listArray (0, length xs - 1) xs
    getPkg = DB.get
    getCs = do
      n   <- DB.get
      nm  <- M.fromList <$> DB.get
      es  <- M.fromList <$> DB.get
      ss  <- M.fromList <$> DB.get
      ls  <- M.fromList <$> DB.get
      pes <- M.fromList <$> DB.get
      pss <- M.fromList <$> DB.get
      pls <- M.fromList <$> DB.get
      CompactorState (dropWhile (/=n) renamedVars)
                             nm
                             es
                             (M.size es)
                             ss
                             (M.size ss)
                             ls
                             (M.size ls)
                             pes
                             pss
                             pls <$> DB.get
    getBase' = do
      hdr <- DB.getByteString 9
      when (hdr /= "GHCJSBASE")
           (panic $ "getBase: invalid base file: " <> file)
      vt  <- DB.getLazyByteString (fromIntegral Object.versionTagLength)
      when (vt /= Object.versionTag)
           (panic $ "getBase: incorrect version: " <> file)
      cs <- makeCompactorParent <$> getCs
      linkedPackages <- getList DB.get
      pkgs <- la <$> getList getPkg
      mods <- la <$> getList DB.get
      funs <- getList (getFun pkgs mods)
      return (Base cs linkedPackages $ S.fromList funs)

-- | make a base state from a CompactorState: empty the current symbols sets,
--   move everything to the parent
makeCompactorParent :: CompactorState -> CompactorState
makeCompactorParent (CompactorState is nm es nes ss nss ls nls pes pss pls sts)
  = CompactorState is
                   nm
                   M.empty 0
                   M.empty 0
                   M.empty 0
                   (M.union (fmap (+nes) pes) es)
                   (M.union (fmap (+nss) pss) ss)
                   (M.union (fmap (+nls) pls) ls)
                   sts

instance DB.Binary Base where
  get = getBase "<unknown file>"
  put = putBase
