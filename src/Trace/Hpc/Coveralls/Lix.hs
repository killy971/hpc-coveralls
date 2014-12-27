-- |
-- Module:      Trace.Hpc.Coveralls.Lix
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for converting hpc output to line-based code coverage data.

module Trace.Hpc.Coveralls.Lix where

import Data.List
import Data.Ord
import Prelude hiding (getLine)
import Trace.Hpc.Coveralls.Types
import Trace.Hpc.Coveralls.Util
import Trace.Hpc.Mix
import Trace.Hpc.Util

toHit :: [Bool] -> Hit
toHit []  = Irrelevant
toHit [x] = if x then Full else None
toHit xs
    | and xs    = Full
    | or xs     = Partial
    | otherwise = None

getLine :: MixEntry -> Int
getLine = fffst . fromHpcPos . fst
    where fffst (x, _, _, _) = x

toLineHit :: CoverageEntry -> (Int, Bool)
toLineHit (entries, counts, _source) = (getLine (head entries) - 1, all (> 0) counts)

isOtherwiseEntry :: CoverageEntry -> Bool
isOtherwiseEntry (mixEntries, _, source) =
    source == ["otherwise"] && boxLabels == otherwiseBoxLabels
    where boxLabels = map snd mixEntries
          otherwiseBoxLabels = [
              ExpBox False,
              BinBox GuardBinBox True,
              BinBox GuardBinBox False]

adjust :: CoverageEntry -> CoverageEntry
adjust coverageEntry@(mixEntries, tixs, source) =
    if isOtherwiseEntry coverageEntry && any (> 0) tixs
    then (mixEntries, [1, 1, 1], source)
    else coverageEntry

-- | Convert hpc coverage entries into a line based coverage format
toLix :: Int             -- ^ Source line count
      -> [CoverageEntry] -- ^ Mix entries and associated hit count
      -> Lix             -- ^ Line coverage
toLix lineCount entries = map toHit (groupByIndex lineCount sortedLineHits)
    where sortedLineHits = sortBy (comparing fst) lineHits
          lineHits = map (toLineHit . adjust) entries
