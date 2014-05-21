module Trace.Hpc.Coveralls.Config where

import Trace.Hpc.Coveralls.Types (CoverageMode)

data Config = Config {
    testSuites   :: [String],
    excludedDirs :: [FilePath],
    mode         :: CoverageMode
    }
