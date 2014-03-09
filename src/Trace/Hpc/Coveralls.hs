{-# LANGUAGE OverloadedStrings #-}

module Trace.Hpc.Coveralls where

import Data.Aeson
import Data.Text
import Trace.Hpc.Tix
import qualified Data.ByteString.Lazy.Char8 as BSL

tixDir :: String
tixDir = "dist/hpc/tix/"

tixToJson :: Tix -> Value
tixToJson _ = object [ "coveralls-support" .= ("TODO" :: Text)]

generateCoverallsFromTix :: String -> IO ()
generateCoverallsFromTix name = do
    mtix <- readTix path
    case mtix of
        Nothing -> error $ "Couldn't find the file " ++ path
        Just tix -> BSL.putStrLn $ encode (tixToJson tix)
    where path = tixDir ++ name ++ "/" ++ getTixFileName name
