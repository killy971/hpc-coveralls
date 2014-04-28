{-# LANGUAGE DeriveDataTypeable #-}

module HpcCoverallsCmdLine where

import Data.List
import Data.Version (Version(..))
import Paths_hpc_coveralls (version)
import System.Console.CmdArgs

data HpcCoverallsArgs = CmdMain
    { excludeDirs   :: [String]
    , testSuites    :: [String]
    , displayReport :: Bool
    } deriving (Data, Show, Typeable)

hpcCoverallsArgs :: HpcCoverallsArgs
hpcCoverallsArgs = CmdMain
    { excludeDirs   = []    &= explicit &= typDir &= name "exclude-dir"    &= help "Exclude sources files under the matching directory from the coverage report send to coveralls.io"
    , displayReport = False &= explicit           &= name "display-report" &= help "Display the json code coverage report that will be sent to coveralls.io"
    , testSuites    = []                &= typ "TEST-SUITE" &= args
    } &= summary ("hpc-coveralls-" ++ versionString version ++ ", (C) Guillaume Nargeot 2014")
      &= program "hpc-coveralls"
    where versionString = intercalate "." . map show . versionBranch
