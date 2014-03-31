module Main where

import System.Exit ( exitFailure, exitSuccess )
import Test.HUnit
import TestHpcLix

allTests :: [Test]
allTests = [testHpcLix]

main :: IO Counts
main = do
    cnt <- runTestTT (test allTests)
    if errors cnt + failures cnt == 0
        then exitSuccess
        else exitFailure
