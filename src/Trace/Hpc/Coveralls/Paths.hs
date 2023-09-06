{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Paths
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants and functions for hpc coverage report output.

module Trace.Hpc.Coveralls.Paths where

import Control.Monad
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import System.Directory (
    doesDirectoryExist, getDirectoryContents, doesFileExist
    )
import System.Directory.Tree (
    AnchoredDirTree(..), dirTree, readDirectoryWith
    )
import Trace.Hpc.Tix
import Trace.Hpc.Coveralls.Types

distDir :: FilePath
distDir = "dist/"

hpcDistDirs :: [FilePath]
hpcDistDirs = map (distDir ++) ["hpc/vanilla/", "hpc/"]

cabalProjectHpcDirs :: FilePath -> [PackageIdentifier] -> [FilePath]
cabalProjectHpcDirs hpcBaseDir = fmap pkgDir 
  where
    pkgDir p = hpcBaseDir <> "/" <> (asNameVer p) <> "/"

tixDir :: String -> FilePath
tixDir = (++ "tix/")

mixDir :: String -> FilePath
mixDir = (++ "mix/")

getMixPath :: Maybe String -- ^ target package name-version
           -> String       -- ^ hpc output base directory
           -> String       -- ^ test suite name
           -> TixModule    -- ^ tix module
           -> FilePath     -- ^ mix file path
getMixPath mPkgNameVer hpcDir testSuiteName tix = mixDir hpcDir ++ dirName ++ "/"
    where dirName = case span (/= '/') modName of
              (_, []) -> testSuiteName
              (packageId, _) -> fromMaybe packageId mPkgNameVer
          TixModule modName _ _ _ = tix

-- | Given a list of hpc data directories, return a list of possible
-- tix file paths for the given test suite.
possibleTixFileLocations :: [FilePath] -> String -> [FilePath]
possibleTixFileLocations hpcDirs testSuiteName = possibleTixFiles
  where
    -- List of possible tix file paths
    possibleTixFiles :: [FilePath]
    possibleTixFiles = tixFileInDir <$> hpcDirs

    -- Path of the tix file in the given hpc directory.
    tixFileInDir :: FilePath -> FilePath
    tixFileInDir hpcDir = tixDir hpcDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName

firstExistingDirectory :: [FilePath] -> IO (Maybe FilePath)
firstExistingDirectory = fmap msum . mapM pathIfExist
    where pathIfExist path = do
              pathExists <- doesDirectoryExist path
              return $ if pathExists then Just path else Nothing

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile = fmap msum . mapM fileIfExist
    where fileIfExist path = do
              fileExists <- doesFileExist path
              return $ if fileExists then Just path else Nothing

dumpDirectory :: FilePath -> IO ()
dumpDirectory path = do
    directoryExists <- doesDirectoryExist path
    unless directoryExists $ putStrLn ("Couldn't find the directory " ++ path)
    putStrLn ("Dumping " ++ path ++ " directory content:")
    contents <- getDirectoryContents path
    traverse putStrLn contents
    return ()

dumpDirectoryTree :: FilePath -> IO ()
dumpDirectoryTree path = do
    putStrLn ("Dumping " ++ path ++ " directory tree:")
    tree <- readDirectoryWith return path
    traverse putStrLn $ dirTree tree
    return ()
