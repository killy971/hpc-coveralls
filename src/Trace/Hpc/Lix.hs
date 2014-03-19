module Trace.Hpc.Lix (Hit (..), Lix, toLix) where

import Data.List
import Data.Ord
import Prelude hiding (getLine)
import Trace.Hpc.Mix
import Trace.Hpc.Util

data Hit = Full | Partial | None | Irrelevant deriving (Show)

type Lix = [Hit]

groupByIndex :: Int -> [(Int, a)] -> [[a]]
groupByIndex size = take size . flip (++) (repeat []) . groupByIndex' 0 []
    where groupByIndex' _ ys [] = [ys]
          groupByIndex' i ys xx@((xi, x) : xs) = if xi == i
              then groupByIndex' i (x : ys) xs
              else ys : groupByIndex' (i + 1) [] xx

toHit :: [Bool] -> Hit
toHit [] = Irrelevant
toHit [x] = if x then Full else None
toHit xs
    | and xs = Full
    | or xs = Partial
    | otherwise = None

getLine :: MixEntry -> Int
getLine = fffst . fromHpcPos . fst
    where fffst (x, _, _, _) = x

toLineHit :: (MixEntry, Integer) -> (Int, Bool)
toLineHit (entry, cnt) = (line - 1, isHit (snd entry))
    where line = getLine entry
          isHit (ExpBox _) = cnt > 0
          isHit (TopLevelBox _) = cnt > 0
          isHit (LocalBox _) = cnt > 0
          isHit (BinBox _ _) = cnt > 0

-- | Convert hpc coverage entries into a line based coverage format
toLix :: Int                   -- ^ Source line count
      -> [(MixEntry, Integer)] -- ^ Mix entries and associated hit count
      -> Lix                   -- ^ Line coverage
toLix lineCount entries = map toHit (groupByIndex lineCount sortedLineHits)
    where sortedLineHits = sortBy (comparing fst) lineHits
          lineHits = map toLineHit entries
