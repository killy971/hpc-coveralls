module Main where

import Trace.Hpc.Coveralls
import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        [path] -> generateCoverallsFromTix path >> exitSuccess
        _ -> usage >> exitSuccess
    where
        usage = putStrLn "Usage: cabal run hpc-coveralls [path]"
