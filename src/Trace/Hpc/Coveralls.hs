{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types and functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( generateCoverallsFromTix ) where

import Data.Aeson
import Data.Aeson.Types ()
import Data.List
import System.Exit (exitFailure)
import Text.Regex.Posix
import Trace.Hpc.Coveralls.Config
import Trace.Hpc.Lix
import Trace.Hpc.Mix
import Trace.Hpc.Tix

type CoverageData = (
    String,    -- source file path
    String,    -- file source code
    Mix,       -- module index data
    TixModule) -- tixs recorded by hpc

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageEntry]

-- Is there a way to restrict this to only Number and Null?
type CoverageEntry = Value

hpcDir :: String
hpcDir = "dist/hpc/"

tixDir :: String
tixDir = hpcDir ++ "tix/"

mixDir :: String
mixDir = hpcDir ++ "mix/"

lixToSimpleCoverage :: Lix -> SimpleCoverage
lixToSimpleCoverage = map conv
    where conv Full    = Number 2
          conv Partial = Number 1
          conv None    = Number 0
          conv Irrelevant = Null

toSimpleCoverage :: Int -> [(MixEntry, Integer)] -> SimpleCoverage
toSimpleCoverage lineCount = lixToSimpleCoverage . toLix lineCount

coverageToJson :: CoverageData -> Value
coverageToJson (filePath, source, mix, tix) = object [
    "name" .= filePath,
    "source" .= source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage lineCount mixEntryTixs
          lineCount = length $ lines source
          mixEntryTixs = zip (getMixEntries mix) (tixModuleTixs tix)
          getMixEntries (Mix _ _ _ _ mixEntries) = mixEntries

toCoverallsJson :: String -> String -> [CoverageData] -> Value
toCoverallsJson serviceName jobId coverageData = object [
    "service_job_id" .= jobId,
    "service_name" .= serviceName,
    "source_files" .= map coverageToJson coverageData]

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (fileName =~) $ map ("^" ++) patterns

-- | Create a list of coverage data from the tix input
toCoverageData :: String            -- ^ test suite name
               -> Tix               -- ^ tix data
               -> [String]          -- ^ excluded source folders
               -> IO [CoverageData] -- ^ coverage data list
toCoverageData testSuiteName (Tix tixs) excludeDirPatterns = do
    mixs <- mapM readMix' tixs
    let files = map filePath mixs
    sources <- mapM readFile files
    let coverageDataList = zip4 files sources mixs tixs
    return $ filter sourceDirFilter coverageDataList
    where readMix' tix = readMix [mixPath] (Right tix)
              where mixPath = mixDir ++ dirName ++ "/"
                    dirName = case span (/= '/') modName of
                        (_, []) -> testSuiteName
                        (packageId, _) -> packageId
                    TixModule modName _ _ _ = tix
          filePath (Mix fp _ _ _ _) = fp
          sourceDirFilter = not . matchAny excludeDirPatterns . fst4
          fst4 (x, _, _, _) = x

-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String   -- ^ CI name
                         -> String   -- ^ CI Job ID
                         -> Config   -- ^ hpc-coveralls configuration
                         -> IO Value -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId config = do
    mtix <- readTix tixPath
    case mtix of
        Nothing -> error ("Couldn't find the file " ++ tixPath) >> exitFailure
        Just tixs -> do
            coverageDatas <- toCoverageData testSuiteName tixs (excludedDirs config)
            return $ toCoverallsJson serviceName jobId coverageDatas
    where tixPath = tixDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName
          testSuiteName = head (testSuiteNames config) -- multiple test suite mode is supported at the moment
