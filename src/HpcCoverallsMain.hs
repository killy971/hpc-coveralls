module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import Data.Maybe
import HpcCoverallsCmdLine
import System.Console.CmdArgs
import System.Environment (getEnv, getEnvironment)
import System.Exit (exitFailure, exitSuccess)
import Trace.Hpc.Coveralls
import Trace.Hpc.Coveralls.Config
import Trace.Hpc.Coveralls.Curl

urlApiV1 :: String
urlApiV1 = "https://coveralls.io/api/v1/jobs"

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

toConfig :: HpcCoverallsArgs -> Maybe Config
toConfig hca = case testSuites hca of
    [testSuite] -> case excludeDirs hca of
        Nothing   -> Just $ Config [testSuite] []
        Just dirs -> Just $ Config [testSuite] dirs
    _ -> Nothing

main :: IO ()
main = do
    hca <- cmdArgs hpcCoverallsArgs
    case toConfig hca of
        Nothing -> putStrLn "Please specify a target test suite name" >> exitSuccess
        Just config -> do
            (serviceName, jobId) <- getServiceAndJobID
            coverallsJson <- generateCoverallsFromTix serviceName jobId config
            let filePath = serviceName ++ "-" ++ jobId ++ ".json"
            writeJson filePath coverallsJson
            response <- postJson filePath urlApiV1
            case response of
                PostSuccess url -> putStrLn ("URL: " ++ url) >> exitSuccess
                PostFailure msg -> putStrLn ("Error: " ++ msg) >> exitFailure
