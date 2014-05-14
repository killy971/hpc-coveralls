module Trace.Hpc.Coveralls.Config where

data Config = Config {
    testSuites   :: [String],
    excludedDirs :: [FilePath]
    }
