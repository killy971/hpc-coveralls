module TestHpcLix where

import Test.HUnit
import Trace.Hpc.Lix

testHpcLix :: Test
testHpcLix = "Lix" ~: [
    groupByIndex 0 [(0, 1 :: Integer)] ~=? []]
