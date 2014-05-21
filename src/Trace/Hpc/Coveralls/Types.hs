{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Types
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent hpc code coverage data.

module Trace.Hpc.Coveralls.Types where

import           Data.Aeson.Types (Value)
import           Data.Data
import qualified Data.Map as M
import           Network.Curl
import           System.Console.CmdArgs.Default
import           Trace.Hpc.Mix

type ModuleCoverageData = (
    String,    -- file source code
    Mix,       -- module index data
    [Integer]) -- tixs recorded by hpc

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageEntry]

-- Is there a way to restrict this to only Number and Null?
type CoverageEntry = Value

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type Lix = [Hit]

instance Default CoverageMode where
    def = AllowPartialLines

data CoverageMode = StrictlyFullLines
                  | AllowPartialLines
    deriving (Data, Eq, Show, Typeable)

-- | Result to the POST request to coveralls.io
data PostResult =
    PostSuccess URLString -- ^ Coveralls job url
  | PostFailure String    -- ^ error message
