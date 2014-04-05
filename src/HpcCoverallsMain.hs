module Main where

import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Exit (exitSuccess)
import Trace.Hpc.Coveralls
import Trace.Hpc.Coveralls.Curl

getServiceAndJobID :: IO (String, String)
getServiceAndJobID = do
    env <- getEnvironment
    case fmap snd $ find (isJust . flip lookup env . fst) ciEnvVars of
        Just (ciName, jobIdVarName) -> do
            jobId <- getEnv jobIdVarName
            return (ciName, jobId)
        _ -> error "Unsupported CI service."
    where ciEnvVars = [
           ("TRAVIS",      ("travis-ci", "TRAVIS_JOB_ID")),
           ("CIRCLECI",    ("circleci",  "CIRCLE_BUILD_NUM")),
           ("SEMAPHORE",   ("semaphore", "REVISION")),
           ("JENKINS_URL", ("jenkins",   "BUILD_ID")),
           ("CI_NAME",     ("codeship",  "CI_BUILD_NUMBER"))]

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
