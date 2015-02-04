{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Paths
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants and functions for hpc coverage report output.

module Trace.Hpc.Coveralls.Paths (
    dumpDirectoryTree,
    getMixPath,
    getTixPath,
    hpcDir
    ) where

import Control.Monad
import Data.Traversable (traverse)
import System.Directory (doesDirectoryExist)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    dirTree, readDirectoryWith
    )
import System.Exit (exitFailure)
import Trace.Hpc.Tix

hpcDir :: FilePath
hpcDir = "dist/hpc/"

tixDir :: FilePath
tixDir = hpcDir ++ "tix/"

mixDir :: FilePath
mixDir = hpcDir ++ "mix/"

getMixPath :: String -> TixModule -> FilePath
getMixPath testSuiteName tix = mixDir ++ dirName ++ "/"
    where dirName = case span (/= '/') modName of
              (_, []) -> testSuiteName
              (packageId, _) -> packageId
          TixModule modName _ _ _ = tix

getTixPath :: String -> FilePath
getTixPath testSuiteName = tixDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName

dumpDirectoryTree :: FilePath -> IO ()
dumpDirectoryTree path = do
    directoryExists <- doesDirectoryExist path
    unless directoryExists $ putStrLn ("Couldn't find the directory " ++ path) >> exitFailure
    tree <- readDirectoryWith return path
    traverse putStrLn $ dirTree tree
    return ()
