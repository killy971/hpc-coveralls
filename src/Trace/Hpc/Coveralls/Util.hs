-- |
-- Module:      Trace.Hpc.Coveralls.Util
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Utility functions.

module Trace.Hpc.Coveralls.Util where

import Data.List

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

toFirstAndRest :: (a, b, c, d) -> (a, (b, c, d))
toFirstAndRest (a, b, c, d) = (a, (b, c, d))

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (`isPrefixOf` fileName) patterns
