module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe hiding (listToMaybe)
import           HpcCoverallsCmdLine
import           System.Console.CmdArgs
import           System.Environment (getEnv, getEnvironment)
import           System.Exit (exitFailure)
import           Trace.Hpc.Coveralls
import           Trace.Hpc.Coverage
import           Trace.Hpc.Coveralls.Config (Config(Config, serviceName, excludedDirs, repoToken, coverageMode))
import           Trace.Hpc.Coveralls.Curl
import           Trace.Hpc.Coveralls.GitInfo (getGitInfo)
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Coveralls.Types

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

getConfig :: HpcCoverallsArgs -> Config
getConfig hca = Config
    (optExcludeDirs hca)
    (optCoverageMode hca)
    (optCabalFile hca)
    (optServiceName hca)
    (optRepoToken hca)
    (optHpcDirs hca)
    (optPackageDirs hca)
    (argTestSuites hca)

main :: IO ()
main = do
  hca <- cmdArgs hpcCoverallsArgs
  let config = getConfig hca

  hpcDirs        <- findHpcDataDirs config
  pkgs           <- findPackages config
  testSuiteNames <- findTestSuiteNames config pkgs
  coverageData   <- getCoverageData pkgs hpcDirs (excludedDirs config) testSuiteNames

  (defaultServiceName, jobId) <- getServiceAndJobID
  let sn = fromMaybe defaultServiceName (serviceName config)
  gitInfo <- getGitInfo

  let
    repoTokenM = repoToken config
    converter = case coverageMode config of
      StrictlyFullLines -> strictConverter
      AllowPartialLines -> looseConverter
    coverallsJson = toCoverallsJson sn jobId repoTokenM gitInfo converter coverageData

  when (optDisplayReport hca) $ BSL.putStrLn $ encode coverallsJson

  let filePath = sn ++ "-" ++ jobId ++ ".json"
  writeJson filePath coverallsJson
  unless (optDontSend hca) $ do
      response <- postJson filePath urlApiV1 (optCurlVerbose hca)
      case response of
          PostSuccess url -> do
              putStrLn ("URL: " ++ url)
              -- wait 10 seconds until the page is available
              threadDelay (10 * 1000 * 1000)
              coverageResult <- readCoverageResult url (optCurlVerbose hca)
              case coverageResult of
                  Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage)
                  Nothing -> putStrLn "Failed to read total coverage"
          PostFailure msg -> do
              putStrLn ("Error: " ++ msg)
              putStrLn ("You can get support at " ++ gitterUrl)
              exitFailure
              where gitterUrl = "https://gitter.im/guillaume-nargeot/hpc-coveralls"
