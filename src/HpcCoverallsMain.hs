module Main where

import Data.Aeson
import Trace.Hpc.Coveralls
import Trace.Hpc.Coveralls.Curl
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Exit (exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSL

getServiceAndJobID :: IO (String, String)
getServiceAndJobID = do
    env <- getEnvironment
    case lookup "TRAVIS" env of
        Just _ -> do
            jobId <- getEnv "TRAVIS_JOB_ID"
            return ("travis-ci", jobId)
        _ -> error "Unsupported CI service."

writeJson :: String -> Value -> IO ()
writeJson filePath = BSL.writeFile filePath . encode

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [testName] -> do
            (serviceName, jobId) <- getServiceAndJobID
            coverallsJson <- generateCoverallsFromTix serviceName jobId testName
            let filePath = serviceName ++ "-" ++ jobId ++ ".json"
            writeJson filePath coverallsJson
            response <- postJson filePath "https://coveralls.io/api/v1/jobs"
            putStrLn response >> exitSuccess
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: hpc-coveralls [testName]"
