{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for collection hpc data.

module Trace.Hpc.Coverage ( getCoverageData ) where

import           Control.Applicative
import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import qualified Data.Map.Strict as M
import           System.Directory (findFile)
import           Trace.Hpc.Coveralls.Paths
import           Trace.Hpc.Coveralls.Types
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix

readMix' :: [PackageIdentifier] -> [FilePath] -> String -> TixModule -> IO Mix
readMix' pkgIds hpcDirs name tix = readMix dirs (Right tix)
    where
      dirs        = nub $ (\p hpcDir -> getMixPath p hpcDir name tix) <$> (Nothing : (Just <$> pkgNameVers)) <*> hpcDirs
      pkgNameVers = asNameVer <$> pkgIds

readTix' :: [FilePath]
         -- ^ HPC data directories
         -> String
         -- ^ Test suite name
         -> IO Tix
         -- ^ Tix
readTix' hpcDirs testSuiteName = do
  let tixFileLocations = possibleTixFileLocations hpcDirs testSuiteName
  mTixPath <- firstExistingFile tixFileLocations

  case mTixPath of
    Nothing      ->
      putStrLn ("Couldn't find any of the possible tix file locations: " ++ show tixFileLocations) >> ioFailure
    Just tixPath -> do
      mTix <- readTix tixPath
      case mTix of
          Nothing         ->
            putStrLn ("Couldn't read the file " ++ tixPath) >> ioFailure
          Just tix -> pure tix

getCoverageData
  :: [Package]
  -- ^ Packages
  -> [FilePath]
  -- ^ HPC data directories
  -> [String]
  -- ^ Excluded source folders
  -> [String]
  -- ^ Test suite names
  -> IO TestSuiteCoverageData
getCoverageData pkgs hpcDirs excludedDirPatterns testSuiteNames = do
  -- For each test suite
  foldFor testSuiteNames $ \testSuiteName -> do

    -- Read the tix file for the test suite
    (Tix tixModules) <- readTix' hpcDirs testSuiteName

    -- For each TixModule in the tix file
    foldFor tixModules $ \tixModule@(TixModule _ _ _ tixs) -> do

      -- Read the mix file
      mix@(Mix filePath _ _ _ _) <- readMix' pkgIds hpcDirs testSuiteName tixModule

      -- Also read the source associated with the mix file, but only if it's not excluded
      if matchAny excludedDirPatterns filePath
        then mempty -- If excluded, we just return monoidal identity
        else do
          -- Find source relative to project sub-directory (e.g. "./", "./my-lib-01")
          projectFilePath <- findProjectSourceFile pkgDirs filePath
          source          <- readFile projectFilePath

          -- Package source up with module mix and tix information, indexed by the file path.
          pure . TestSuiteCoverageData $ M.singleton projectFilePath (source, mix, tixs)

          -- Sum all this up using the Monoid instance for TestCoverageData.

  where
    pkgIds  = pkgId <$> pkgs
    pkgDirs = pkgRootDir <$> pkgs

findProjectSourceFile :: [FilePath] -> FilePath -> IO FilePath
findProjectSourceFile pkgDirs fp = do
  mFile <- findFile pkgDirs fp
  case mFile of
    Nothing ->
      putStrLn ("Couldn't find the source file " ++ fp ++ " in directories: " <> show pkgDirs <> ".") >> ioFailure
    (Just actualFilePath) ->
      pure (removeLeading "./" $ -- To retain consistency with current reports
            actualFilePath)
  where
    -- Remove prefix from a string (if present, do nothing otherwise)
    removeLeading :: String -> String -> String
    removeLeading prefix path = fromMaybe path $ stripPrefix prefix path
