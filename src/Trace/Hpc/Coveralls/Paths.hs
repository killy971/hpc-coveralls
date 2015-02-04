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
    dumpDirectory,
    dumpDirectoryTree,
    getMixPath,
    getTixPath,
    hpcDir
    ) where

import Control.Monad
import Data.Traversable (traverse)
import System.Directory (
    doesDirectoryExist, getDirectoryContents
    )
import System.Directory.Tree (
    AnchoredDirTree(..), dirTree, readDirectoryWith
    )
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

dumpDirectory :: FilePath -> IO ()
dumpDirectory path = do
    directoryExists <- doesDirectoryExist path
    unless directoryExists $ putStrLn ("Couldn't find the directory " ++ path)
    putStrLn ("Dumping " ++ path ++ " directory content:")
    contents <- getDirectoryContents path
    traverse putStrLn contents
    return ()

dumpDirectoryTree :: FilePath -> IO Bool
dumpDirectoryTree path = do
    directoryExists <- doesDirectoryExist path
    if directoryExists
        then do
            putStrLn ("Dumping " ++ path ++ " directory tree:")
            tree <- readDirectoryWith return path
            traverse putStrLn $ dirTree tree
            return True
        else putStrLn ("Couldn't find the directory " ++ path) >> return False
