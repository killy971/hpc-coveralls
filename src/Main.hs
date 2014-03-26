module Main where

import Data.Aeson
import Network.Curl
import Trace.Hpc.Coveralls
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSL

writeJson :: String -> Value -> IO ()
writeJson filePath = BSL.writeFile filePath . encode

httpPost :: String -> [HttpPost]
httpPost path = [HttpPost "json_file" Nothing (ContentFile path) [] Nothing]

showResponse :: CurlResponse -> String
showResponse r = show (respCurlCode r) ++ show (respBody r)

postJson :: String -> URLString -> IO String
postJson path url = do
    h <- initialize
    _ <- setopt h (CurlVerbose True)
    _ <- setopt h (CurlURL url)
    _ <- setopt h (CurlHttpPost $ httpPost path)
    r <- perform_with_response_ h
    return $ showResponse r

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [serviceName, jobId, path] -> do
            coverallsJson <- generateCoverallsFromTix serviceName jobId path
            writeJson filePath coverallsJson
            postJson filePath "https://coveralls.io/api/v1/jobs" >> exitSuccess
                where filePath = serviceName ++ "-" ++ jobId ++ ".json"
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: cabal run hpc-coveralls [serviceName] [jobId] [path]"
