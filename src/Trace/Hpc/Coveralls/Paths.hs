-- |
-- Module:      Trace.Hpc.Coveralls.Paths
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants and functions for hpc coverage report output.

module Trace.Hpc.Coveralls.Paths where

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

getTixPath :: String -> IO FilePath
getTixPath testSuiteName = return $ tixDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName
