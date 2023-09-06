{-# LANGUAGE CPP #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Cabal
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for reading cabal package name and version.

module Trace.Hpc.Coveralls.Cabal (getPackageId, getPackageNameVersion, getPackageFromDir, getPackages, readTestSuiteNames) where

import Control.Applicative
import Control.Monad
import Data.List (intercalate, isSuffixOf)
import Data.Semigroup ((<>))
import Distribution.Package (unPackageName, pkgName, pkgVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Version
import System.Directory
import Trace.Hpc.Coveralls.Types

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile dir = do
    cabalFilesInDir <- filter isCabal <$> getDirectoryContents dir
    cabalFiles <- filterM doesFileExist (mkFullPath <$> cabalFilesInDir)
    case cabalFiles of
        [file] -> do
          return $ Just file
        _ -> do
          return Nothing
    where
      isCabal filename = ".cabal" `isSuffixOf` filename && length filename > 6
      mkFullPath = ((dir <> "/") <>)

getPackageNameVersion :: FilePath -> IO (Maybe String)
getPackageNameVersion file = do
    orig <- readFile file
    case parsePackageDescription orig of
        ParseFailed _ -> return Nothing
        ParseOk _warnings gpd -> return $ Just $ name ++ "-" ++ version
            where pkg = package . packageDescription $ gpd
                  name = unPackageName $ pkgName pkg
                  version = showVersion (pkgVersion pkg)
                  showVersion = intercalate "." . map show . versionNumbers

getPackageId :: FilePath -> IO (Maybe PackageIdentifier)
getPackageId cabalFile = do
  orig <- readFile cabalFile
  case parsePackageDescription orig of
    ParseFailed _ -> return Nothing
    ParseOk _warnings gpd -> return . Just $ PackageIdentifier name version
      where pkg = package . packageDescription $ gpd
            name = unPackageName $ pkgName pkg
            version = showVersion (pkgVersion pkg)
            showVersion = intercalate "." . map show . versionNumbers

getPackageFromDir :: FilePath -> IO (Maybe Package)
getPackageFromDir dir = do
  exists <- doesDirectoryExist dir
  if exists == False
    then pure Nothing
    else do
      mCabalFilePath <- getCabalFile dir
      case mCabalFilePath of
        Nothing            -> pure Nothing
        Just cabalFilePath -> do
          mPkgId <- getPackageId cabalFilePath
          pure $ Package dir cabalFilePath <$> mPkgId

-- | Get a list of packages.
--
-- This function works by finding cabal files and parsing them to
-- provide package descriptions. You can provide either a full cabal
-- file paths (for legacy reasons) or directories containing cabal
-- files. Both will be used to generate a list of packages. If you
-- provide none, the current directory will be searched.
getPackages
  :: FindPackageRequest
  -> IO [Package]
getPackages = foldr foldF (pure []) 
  where
    foldF :: (FilePath, Maybe FilePath) -> IO [Package] -> IO [Package]
    foldF x acc  = do
      mPkg <- iter x
      case mPkg of
        Nothing  -> acc
        Just pkg -> (pkg:) <$> acc

    iter :: (FilePath, Maybe FilePath) -> IO (Maybe Package)
    iter (rootDir, Just cabalFilePath) = do
      mPkgId <- getPackageId cabalFilePath
      pure $ Package rootDir cabalFilePath <$> mPkgId
    iter (rootDir, Nothing) = do
      mPkg <- getPackageFromDir rootDir
      pure mPkg

#if !(MIN_VERSION_Cabal(1,22,0))
unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
#endif

#if !(MIN_VERSION_Cabal(2,0,0))
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif

readTestSuiteNames :: FilePath -> IO [String]
readTestSuiteNames cabalFile = do
  contents <- readFile cabalFile
  case parsePackageDescription contents of
    ParseFailed _ -> return []
    ParseOk _warnings gpd -> return $ getTestSuiteNames gpd
  
getTestSuiteNames :: GenericPackageDescription -> [String]
getTestSuiteNames = foldMap ((:[]) . unUnqualComponentName . fst) . condTestSuites
