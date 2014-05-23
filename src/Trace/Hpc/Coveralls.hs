{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( generateCoverallsFromTix ) where

import           Data.Aeson
import           Data.Aeson.Types ()
import           Data.List
import qualified Data.Map.Strict as M
import           System.Exit (exitFailure)
import           Trace.Hpc.Coveralls.Config
import           Trace.Hpc.Coveralls.Lix
import           Trace.Hpc.Coveralls.Paths
import           Trace.Hpc.Coveralls.Types
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix

lixToSimpleCoverage :: CoverageMode -> Lix -> SimpleCoverage
lixToSimpleCoverage mode = map conv
    where conv Full    = case mode of
              StrictlyFullLines -> Number 1
              AllowPartialLines -> Number 2
          conv Partial = case mode of
              StrictlyFullLines -> Number 1
              AllowPartialLines -> Number 0
          conv None    = Number 0
          conv Irrelevant = Null

toSimpleCoverage :: CoverageMode -> Int -> [(MixEntry, Integer)] -> SimpleCoverage
toSimpleCoverage mode lineCount = lixToSimpleCoverage mode . toLix lineCount

coverageToJson :: CoverageMode -> FilePath -> ModuleCoverageData -> Value
coverageToJson mode filePath (source, mix, tixs) = object [
    "name" .= filePath,
    "source" .= source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage mode lineCount mixEntryTixs
          lineCount = length $ lines source
          mixEntryTixs = zip (getMixEntries mix) tixs
          getMixEntries (Mix _ _ _ _ mixEntries) = mixEntries

toCoverallsJson :: String -> String -> CoverageMode -> TestSuiteCoverageData -> Value
toCoverallsJson serviceName jobId mode testSuiteCoverageData = object [
    "service_job_id" .= jobId,
    "service_name" .= serviceName,
    "source_files" .= toJsonCoverageList testSuiteCoverageData]
    where toJsonCoverageList = map (uncurry $ coverageToJson mode) . M.toList

mergeModuleCoverageData :: ModuleCoverageData -> ModuleCoverageData -> ModuleCoverageData
mergeModuleCoverageData (source, mix, tixs1) (_, _, tixs2) =
    (source, mix, zipWith (+) tixs1 tixs2)

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = foldr1 (M.unionWith mergeModuleCoverageData)

readMix' :: String -> TixModule -> IO Mix
readMix' name tix = readMix [getMixPath name tix] (Right tix)

-- | Create a list of coverage data from the tix input
readCoverageData :: String                   -- ^ test suite name
                 -> [String]                 -- ^ excluded source folders
                 -> IO TestSuiteCoverageData -- ^ coverage data list
readCoverageData testSuiteName excludeDirPatterns = do
    tixPath <- getTixPath testSuiteName
    mtix <- readTix tixPath
    case mtix of
        Nothing -> error ("Couldn't find the file " ++ tixPath) >> exitFailure
        Just (Tix tixs) -> do
            mixs <- mapM (readMix' testSuiteName) tixs
            let files = map filePath mixs
            sources <- mapM readFile files
            let coverageDataList = zip4 files sources mixs (map tixModuleTixs tixs)
            let filteredCoverageDataList = filter sourceDirFilter coverageDataList
            return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4

-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String   -- ^ CI name
                         -> String   -- ^ CI Job ID
                         -> Config   -- ^ hpc-coveralls configuration
                         -> IO Value -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId config = do
    testSuitesCoverages <- mapM (`readCoverageData` excludedDirPatterns) testSuiteNames
    return $ toCoverallsJson serviceName jobId mode $ mergeCoverageData testSuitesCoverages
    where excludedDirPatterns = excludedDirs config
          testSuiteNames = testSuites config
          mode = coverageMode config
