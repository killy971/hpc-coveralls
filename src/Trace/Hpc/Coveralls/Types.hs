-- |
-- Module:      Trace.Hpc.Coveralls.Types
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent code coverage data hpc.

module Trace.Hpc.Coveralls.Types where

import Data.Aeson.Types (Value)
import qualified Data.Map as M
import Trace.Hpc.Mix
import Trace.Hpc.Tix

type ModuleCoverageData = (
    String,    -- file source code
    Mix,       -- module index data
    TixModule) -- tixs recorded by hpc

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageEntry]

-- Is there a way to restrict this to only Number and Null?
type CoverageEntry = Value

