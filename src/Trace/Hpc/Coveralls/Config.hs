module Trace.Hpc.Coveralls.Config where

data Config = Config {
    testSuiteNames :: [String],
    excludedDirs   :: [String]
    }
