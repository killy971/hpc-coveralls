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
import qualified Data.Map as M
import System.Exit (exitFailure)
import Text.Regex.Posix
import Trace.Hpc.Coveralls.Config
import Trace.Hpc.Coveralls.Types
import Trace.Hpc.Lix
import Trace.Hpc.Mix
import Trace.Hpc.Paths
import Trace.Hpc.Tix

lixToSimpleCoverage :: Lix -> SimpleCoverage
lixToSimpleCoverage = map conv
    where conv Full    = Number 2
          conv Partial = Number 1
          conv None    = Number 0
          conv Irrelevant = Null

toSimpleCoverage :: Int -> [(MixEntry, Integer)] -> SimpleCoverage
toSimpleCoverage lineCount = lixToSimpleCoverage . toLix lineCount

coverageToJson :: FilePath -> ModuleCoverageData -> Value
coverageToJson filePath (source, mix, tix) = object [
    "name" .= filePath,
    "source" .= source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage lineCount mixEntryTixs
          lineCount = length $ lines source
          mixEntryTixs = zip (getMixEntries mix) (tixModuleTixs tix)
          getMixEntries (Mix _ _ _ _ mixEntries) = mixEntries

toCoverallsJson :: String -> String -> TestSuiteCoverageData -> Value
toCoverallsJson serviceName jobId testSuiteCoverageData = object [
    "service_job_id" .= jobId,
    "service_name" .= serviceName,
    "source_files" .= toJsonCoverageList testSuiteCoverageData]
    where toJsonCoverageList = map (uncurry coverageToJson) . M.toList

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (fileName =~) $ map ("^" ++) patterns

getMixPath :: String -> String -> FilePath
getMixPath testSuiteName modName = mixDir ++ dirName ++ "/"
    where dirName = case span (/= '/') modName of
              (_, []) -> testSuiteName
              (packageId, _) -> packageId

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = head -- for the moment just use the first item

readMix' :: String -> TixModule -> IO Mix
readMix' name tix = readMix [mixPath] (Right tix)
    where mixPath = getMixPath name modName
          TixModule modName _ _ _ = tix

getTixPath :: String -> IO FilePath
getTixPath testSuiteName = return $ tixDir ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName

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
            let coverageDataList = zip4 files sources mixs tixs
            let filteredCoverageDataList = filter sourceDirFilter coverageDataList
            return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4
                  fst4 (x, _, _, _) = x
                  toFirstAndRest (a, b, c, d) = (a, (b, c, d))

-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String   -- ^ CI name
                         -> String   -- ^ CI Job ID
                         -> Config   -- ^ hpc-coveralls configuration
                         -> IO Value -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId config = do
    testSuitesCoverages <- mapM (`readCoverageData` excludedDirPatterns) testSuiteNames
    return $ toCoverallsJson serviceName jobId $ mergeCoverageData testSuitesCoverages
    where excludedDirPatterns = excludedDirs config
          testSuiteNames = testSuites config
