module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe
import           HpcCoverallsCmdLine
import           System.Console.CmdArgs
import           System.Environment (getEnv, getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           Trace.Hpc.Coveralls
import           Trace.Hpc.Coveralls.Config (Config(Config))
import           Trace.Hpc.Coveralls.Curl

urlApiV1 :: String
urlApiV1 = "https://coveralls.io/api/v1/jobs"

getServiceAndJobID :: IO (String, String)
getServiceAndJobID = do
    env <- getEnvironment
    case snd <$> find (isJust . flip lookup env . fst) ciEnvVars of
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

getConfig :: HpcCoverallsArgs -> Maybe Config
getConfig hca = case testSuites hca of
    []             -> Nothing
    testSuiteNames -> Just $ Config testSuiteNames (excludeDirs hca) (coverageMode hca)

main :: IO ()
main = do
    hca <- cmdArgs hpcCoverallsArgs
    case getConfig hca of
        Nothing -> putStrLn "Please specify a target test suite name" >> exitSuccess
        Just config -> do
            (serviceName, jobId) <- getServiceAndJobID
            coverallsJson <- generateCoverallsFromTix serviceName jobId config
            when (displayReport hca) $ BSL.putStrLn $ encode coverallsJson
            let filePath = serviceName ++ "-" ++ jobId ++ ".json"
            writeJson filePath coverallsJson
            unless (dontSend hca) $ do
                response <- postJson filePath urlApiV1 (printResponse hca)
                case response of
                    PostSuccess url -> do
                        putStrLn ("URL: " ++ url)
                        -- wait 10 seconds until the page is available
                        threadDelay (10 * 10000000)
                        coverageResult <- readCoverageResult url (printResponse hca)
                        case coverageResult of
                            Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage) >> exitSuccess
                            Nothing -> exitSuccess
                    PostFailure msg -> putStrLn ("Error: " ++ msg) >> exitFailure
