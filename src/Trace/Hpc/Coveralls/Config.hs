module Trace.Hpc.Coveralls.Config where

import Trace.Hpc.Coveralls.Types (CoverageMode)

data Config = Config {
    excludedDirs   :: ![FilePath],
    coverageMode   :: !CoverageMode,
    cabalFile      :: !(Maybe FilePath),
    serviceName    :: !(Maybe String),
    repoToken      :: !(Maybe String),
    hpcDirOverride :: !(Maybe String),
    testSuites     :: ![String]
    }
