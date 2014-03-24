module Main where

import Data.Aeson
import Trace.Hpc.Coveralls
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as BSL

writeJson :: String -> Value -> IO ()
writeJson filePath = BSL.writeFile filePath . encode

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [serviceName, jobId, path] -> do
            coverallsJson <- generateCoverallsFromTix serviceName jobId path
            writeJson filePath coverallsJson >> exitSuccess
                where filePath = serviceName ++ "-" ++ jobId ++ ".json"
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: cabal run hpc-coveralls [serviceName] [jobId] [path]"
