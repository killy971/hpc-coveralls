{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( toCoverallsJson
                           , strictConverter
                           , looseConverter
                           ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Digest.Pure.MD5
import           Data.Function
import           Data.List
import           Data.Traversable (for)
import           Data.Semigroup (Semigroup((<>)))
import qualified Data.Map.Strict as M
import           Trace.Hpc.Coveralls.GitInfo (GitInfo)
import           Trace.Hpc.Coveralls.Lix
import           Trace.Hpc.Coveralls.Types
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Mix
import           Trace.Hpc.Util

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageValue]

-- Is there a way to restrict this to only Number and Null?
type CoverageValue = Value

type LixConverter = Lix -> SimpleCoverage

strictConverter :: LixConverter
strictConverter = map $ \lix -> case lix of
    Full       -> Number 1
    Partial    -> Number 0
    None       -> Number 0
    Irrelevant -> Null

looseConverter :: LixConverter
looseConverter = map $ \lix -> case lix of
    Full       -> Number 2
    Partial    -> Number 1
    None       -> Number 0
    Irrelevant -> Null

toSimpleCoverage :: LixConverter -> Int -> [CoverageEntry] -> SimpleCoverage
toSimpleCoverage convert lineCount = convert . toLix lineCount

getExprSource :: [String] -> MixEntry -> [String]
getExprSource source (hpcPos, _) = subSubSeq startCol endCol subLines
    where subLines = subSeq startLine endLine source
          startLine = startLine' - 1
          startCol = startCol' - 1
          (startLine', startCol', endLine, endCol) = fromHpcPos hpcPos

groupMixEntryTixs :: [(MixEntry, Integer, [String])] -> [CoverageEntry]
groupMixEntryTixs = map mergeOnLst3 . groupBy ((==) `on` fst . fst3)
    where mergeOnLst3 xxs@(x : _) = (map fst3 xxs, map snd3 xxs, trd3 x)
          mergeOnLst3 [] = error "mergeOnLst3 appliedTo empty list"

coverageToJson :: LixConverter -> FilePath -> ModuleCoverageData -> Value
coverageToJson converter filePath (source, mix, tixs) = object [
    "name" .= filePath,
    "source_digest" .= (show . md5 . LBS.pack) source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage converter lineCount mixEntriesTixs
          lineCount = length $ lines source
          mixEntriesTixs = groupMixEntryTixs mixEntryTixs
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ lines source

toCoverallsJson :: String -> String -> Maybe String -> GitInfo -> LixConverter -> TestSuiteCoverageData -> Value
toCoverallsJson serviceName jobId repoTokenM gitInfo converter (TestSuiteCoverageData testSuiteCoverageData) =
    object $ if serviceName == "travis-ci" then withRepoToken else withGitInfo
    where base = [
              "service_job_id" .= jobId,
              "service_name" .= serviceName,
              "source_files" .= toJsonCoverageList testSuiteCoverageData]
          toJsonCoverageList = map (uncurry $ coverageToJson converter) . M.toList
          withRepoToken = mcons (("repo_token" .=) <$> repoTokenM) base
          withGitInfo   = ("git" .= gitInfo) : withRepoToken
