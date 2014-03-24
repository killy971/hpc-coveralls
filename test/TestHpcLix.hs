{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestHpcLix where

import Test.HUnit
import Trace.Hpc.Lix

testGroupByIndex = "groupByIndex" ~: [
    groupByIndex 0 [(0, 2)] @?= [],
    groupByIndex 1 [(0, 2)] @?= [[2]],
    groupByIndex 2 [(0, 2)] @?= [[2], []],
    groupByIndex 0 [(1, 2)] @?= [],
    groupByIndex 1 [(1, 2)] @?= [[]],
    groupByIndex 2 [(1, 2)] @?= [[], [2]],
    groupByIndex 3 [(1, 2)] @?= [[], [2], []],
    groupByIndex 0 [(0, 2), (0, 3)] @?= [],
    groupByIndex 1 [(0, 2), (0, 3)] @?= [[3, 2]],
    groupByIndex 1 [(0, 2), (1, 3)] @?= [[2]],
    groupByIndex 1 [(1, 2), (1, 3)] @?= [[]],
    groupByIndex 2 [(0, 2), (0, 3)] @?= [[3, 2], []],
    groupByIndex 2 [(0, 2), (1, 3)] @?= [[2], [3]],
    groupByIndex 2 [(1, 2), (1, 3)] @?= [[], [3, 2]],
    groupByIndex 3 [(0, 2), (0, 3)] @?= [[3, 2], [], []],
    groupByIndex 3 [(0, 2), (1, 3)] @?= [[2], [3], []],
    groupByIndex 3 [(1, 2), (1, 3)] @?= [[], [3, 2], []],
    groupByIndex 5 [(0, 2), (2, 5), (2, 3), (4, 13), (4, 11), (4, 7)] @?= [[2], [], [3, 5], [], [7, 11, 13]]]

testToHit = "toHit" ~: [
    Irrelevant @=? toHit [],
    None       @=? toHit [False],
    None       @=? toHit [False, False],
    Partial    @=? toHit [False, True],
    Partial    @=? toHit [True, False],
    Partial    @=? toHit [False, False, True],
    Partial    @=? toHit [False, True, False],
    Partial    @=? toHit [True, False, False],
    Full       @=? toHit [True],
    Full       @=? toHit [True, True]]

testHpcLix = "Lix" ~: [testGroupByIndex, testToHit]
