{-# LANGUAGE OverloadedStrings #-}

module Trace.Hpc.Coveralls where

import Data.Aeson
import Data.Aeson.Types ()
import Trace.Hpc.Lix
import Trace.Hpc.Mix
import Trace.Hpc.Tix

type CoverageData = (
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
coverageToJson (source, mix, tix) = object [
    "name" .= getFilePath mix,
    "source" .= source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage lineCount mixEntryTixs
          lineCount = length $ lines source
          mixEntryTixs = zip (getMixEntries mix) (tixModuleTixs tix)
          getMixEntries (Mix _ _ _ _ mixEntries) = mixEntries
          getFilePath (Mix filePath _ _ _ _) = filePath

toCoverallsJson :: String -> String -> [CoverageData] -> Value
toCoverallsJson serviceName jobId coverageData = object [
    "service_job_id" .= jobId,
    "service_name" .= serviceName,
    "source_files" .= map coverageToJson coverageData]

toCoverageData :: String -> Tix -> IO [CoverageData]
toCoverageData name (Tix tixs) = do
    mixs <- mapM readMix' tixs
    sources <- mapM readSource mixs
    return $ zip3 sources mixs tixs
    where readMix' tix = readMix [mixPath] (Right tix)
          mixPath = mixDir ++ name ++ "/"
          readSource (Mix filePath _ _ _ _) = readFile filePath

-- |
generateCoverallsFromTix :: String -- ^
                         -> String -- ^
                         -> String -- ^
                         -> IO Value
generateCoverallsFromTix serviceName jobId name = do
    mtix <- readTix tixPath
    case mtix of
        Nothing -> error $ "Couldn't find the file " ++ tixPath
        Just tixs -> do
            coverageDatas <- toCoverageData name tixs
            return $ toCoverallsJson serviceName jobId coverageDatas
    where tixPath = tixDir ++ name ++ "/" ++ getTixFileName name
