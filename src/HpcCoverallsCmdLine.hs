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
    , optCabalFile     :: Maybe String
    , optTixDirPath    :: Maybe String
    , optMixDirPath    :: Maybe String
    , optUseStackCov   :: Bool
    , optServiceName   :: Maybe String
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
    , optCurlVerbose   = False             &= explicit                &= name "curl-verbose"   &= help "Enable curl verbose mode and prints the json response received from coveralls.io"
    , optDontSend      = False             &= explicit                &= name "dont-send"      &= help "Do not send the report to coveralls.io"
    , optCoverageMode  = AllowPartialLines &= explicit &= typ "MODE"  &= name "coverage-mode"  &= help "Coverage conversion mode: AllowPartialLines (default), StrictlyFullLines"
    , optCabalFile     = Nothing           &= explicit &= typ "FILE"  &= name "cabal-file"     &= help "Cabal file (ex.: module-name.cabal)"
    , optTixDirPath    = Nothing           &= explicit &= typDir      &= name "tix-dir"        &= help "Tix dir (ex.: dist/hpc/tix/)"
    , optMixDirPath    = Nothing           &= explicit &= typDir      &= name "mix-dir"        &= help "Mix dir (ex.: dist/hpc/mix/)"
    , optUseStackCov   = False             &= explicit                &= name "use-stack-cov"  &= help "Use the stack coverage report"
    , optServiceName   = Nothing           &= explicit &= typ "TOKEN" &= name "service-name"   &= help "service-name (e.g. travis-pro)"
    , optRepoToken     = Nothing           &= explicit &= typ "TOKEN" &= name "repo-token"     &= help "Coveralls repo token"
    , argTestSuites    = []                &= typ "TEST-SUITES" &= args
    } &= summary ("hpc-coveralls v" ++ versionString version ++ ", (C) Guillaume Nargeot 2014-2015")
      &= program "hpc-coveralls"
    where versionString = intercalate "." . map show . versionBranch
