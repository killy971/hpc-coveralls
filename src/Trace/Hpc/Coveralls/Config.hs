module Trace.Hpc.Coveralls.Config where

import Trace.Hpc.Coveralls.Types (Mode)

data Config = Config {
    testSuites   :: [String],
    excludedDirs :: [FilePath],
    mode         :: Mode
    }
