module TestHpcCoveralls where

import Data.Aeson
import Data.Aeson.Types ()
import Test.HUnit
import Trace.Hpc.Coveralls
import Trace.Hpc.Lix

testHpcCoveralls :: Test
testHpcCoveralls = "Coveralls" ~: [
    lixToSimpleCoverage [Irrelevant] ~=? [Null]]
