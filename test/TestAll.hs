module Main where

import System.Exit ( exitFailure, exitSuccess )
import Test.HUnit

testHpcCoveralls :: Test
testHpcCoveralls = "HPC-Coveralls" ~: [
    True ~=? True]

allTests :: [Test]
allTests = [testHpcCoveralls]

main :: IO Counts
main = do
    cnt <- runTestTT (test allTests)
    if errors cnt + failures cnt == 0
        then exitSuccess
        else exitFailure

