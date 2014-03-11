{-# LANGUAGE OverloadedStrings #-}

module Trace.Hpc.Coveralls where

import Data.Aeson
import Data.Aeson.Types (emptyArray)
import Data.String.Utils (replace)
import Data.Text (pack)
import Trace.Hpc.Tix
import qualified Data.ByteString.Lazy.Char8 as BSL

tixDir :: String
tixDir = "dist/hpc/tix/"

-- is there a better way?
-- (this is not correct as some files are under src/ and others under test/)
toFilename :: String -> String
toFilename = (++ ".hs") . replace "." "/"

-- tixs accessor: tixModuleTicks
tixModuleToJson :: TixModule -> Value
tixModuleToJson tix = object [
    "name" .= filename,
    "source" .= pack "", -- empty for now
    "coverage" .= emptyArray] -- empty for now
    where moduleName = tixModuleName tix
          filename = pack $ toFilename moduleName

tixToJson :: Tix -> Value
tixToJson (Tix tixModules) = object [
    "service_job_id" .= pack "0", -- dummy id for now
    "service_name" .= pack "travis-ci", -- fixed for now
    "source_files" .= resultList]
    where resultList = map tixModuleToJson tixModules

generateCoverallsFromTix :: String -> IO ()
generateCoverallsFromTix name = do
    mtix <- readTix path
    case mtix of
        Nothing -> error $ "Couldn't find the file " ++ path
        Just tix -> BSL.putStrLn $ encode (tixToJson tix)
    where path = tixDir ++ name ++ "/" ++ getTixFileName name
