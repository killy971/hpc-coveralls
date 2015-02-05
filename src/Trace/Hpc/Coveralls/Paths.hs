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
-- import Data.Foldable
import Data.Traversable (traverse)
import System.Directory (
    doesDirectoryExist, getDirectoryContents
    )
import System.Directory.Tree (
    AnchoredDirTree(..), dirTree, readDirectoryWith
    )
import Trace.Hpc.Tix

distDir :: FilePath
distDir = "dist/"

hpcDirs :: [FilePath]
hpcDirs = map (distDir ++) ["hpc/vanilla/", "hpc/"]

tixDir :: String -> FilePath
tixDir = (++ "tix/")

mixDir :: String -> FilePath
mixDir = (++ "mix/")

getMixPath :: String -> String -> TixModule -> FilePath
getMixPath hpcDir testSuiteName tix = mixDir hpcDir ++ dirName ++ "/"
    where dirName = case span (/= '/') modName of
              (_, []) -> testSuiteName
              (packageId, _) -> packageId
          TixModule modName _ _ _ = tix

getTixPath :: String -> String -> FilePath
getTixPath hpcDir testSuiteName = tixDir hpcDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName

firstExistingDirectory :: [FilePath] -> IO (Maybe FilePath)
firstExistingDirectory = fmap msum . mapM pathIfExist
    where pathIfExist path = do
              pathExists <- doesDirectoryExist path
              return $ if pathExists then Just path else Nothing

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
