-- |
-- Module:      Trace.Hpc.Paths
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants for hpc coverage report output.

module Trace.Hpc.Paths where

hpcDir :: FilePath
hpcDir = "dist/hpc/"

tixDir :: FilePath
tixDir = hpcDir ++ "tix/"

mixDir :: FilePath
mixDir = hpcDir ++ "mix/"
