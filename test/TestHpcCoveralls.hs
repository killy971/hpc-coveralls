{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestHpcCoveralls where

import Data.Aeson
import Data.Aeson.Types ()
import Test.HUnit
import Trace.Hpc.Coveralls
import Trace.Hpc.Lix

testHpcCoveralls = "Coveralls" ~: [
    lixToSimpleCoverage [Irrelevant] ~=? [Null]]
