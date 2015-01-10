-- |
-- Module:      Trace.Hpc.Coveralls.Util
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Utility functions.

module Trace.Hpc.Coveralls.Util where

import Data.List

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

toFirstAndRest :: (a, b, c, d) -> (a, (b, c, d))
toFirstAndRest (a, b, c, d) = (a, (b, c, d))

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (`isPrefixOf` fileName) patterns

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f (x : xs) = f x : xs
mapFirst _ []       = []
 
mapLast :: (a -> a) -> [a] -> [a]
mapLast f [x]      = [f x]
mapLast f (x : xs) = x : mapLast f xs
mapLast _ []       = []

subSeq :: Int -> Int -> [a] -> [a]
subSeq start end = drop start . take end

subSubSeq :: Int -> Int -> [[a]] -> [[a]]
subSubSeq start end = mapFirst (drop start) . mapLast (take end)

groupByIndex :: Int -> [(Int, a)] -> [[a]]
groupByIndex size = take size . flip (++) (repeat []) . groupByIndex' 0 []
    where groupByIndex' _ ys [] = [ys]
          groupByIndex' i ys xx@((xi, x) : xs) = if xi == i
              then groupByIndex' i (x : ys) xs
              else ys : groupByIndex' (i + 1) [] xx
