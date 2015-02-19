{-# LANGUAGE DeriveDataTypeable #-}

module HpcCoverallsCmdLine where

import Data.List
import Data.Version (Version(..))
import Paths_hpc_coveralls (version)
import System.Console.CmdArgs
import Trace.Hpc.Coveralls.Types

data HpcCoverallsArgs = CmdMain
    { optExcludeDirs   :: [String]
    , argTestSuites    :: [String]
    , optRepoToken     :: Maybe String
    , optDisplayReport :: Bool
    , optCurlVerbose   :: Bool
    , optDontSend      :: Bool
    , optCoverageMode  :: CoverageMode
    } deriving (Data, Show, Typeable)

hpcCoverallsArgs :: HpcCoverallsArgs
hpcCoverallsArgs = CmdMain
    { optExcludeDirs   = []                &= explicit &= typDir      &= name "exclude-dir"    &= help "Exclude sources files under the matching directory from the coverage report"
    , optDisplayReport = False             &= explicit                &= name "display-report" &= help "Display the json code coverage report that will be sent to coveralls.io"
    , optCurlVerbose   = False             &= explicit                &= name "curl-verbose"   &= help "Enable curl verbose mode and Prints the json response received from coveralls.io"
    , optDontSend      = False             &= explicit                &= name "dont-send"      &= help "Do not send the report to coveralls.io"
    , optCoverageMode  = AllowPartialLines &= explicit &= typ "MODE"  &= name "coverage-mode"  &= help "Coverage conversion mode: AllowPartialLines (default), StrictlyFullLines"
    , optRepoToken     = Nothing           &= explicit &= typ "TOKEN" &= name "repo-token"     &= help "Coveralls repo token"
    , argTestSuites    = []                &= typ "TEST-SUITES" &= args
    } &= summary ("hpc-coveralls-" ++ versionString version ++ ", (C) Guillaume Nargeot 2014-2015")
      &= program "hpc-coveralls"
    where versionString = intercalate "." . map show . versionBranch
