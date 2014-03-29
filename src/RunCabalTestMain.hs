module Main where

import Control.Monad
import GHC.IO.Handle
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Process
import Text.Regex.Posix

isTestFailure :: String -> Bool
isTestFailure line = line =~ "^Test suite .*: FAIL$"

readLines :: Handle -> IO [String]
readLines h = do
    isEOF <- hIsEOF h
    if isEOF
        then return []
        else do
            x <- hGetLine h
            putStrLn x
            xs <- readLines h
            return (x : xs)

runCabalTest :: [String] -> IO Bool
runCabalTest args = do
    (_, out, _, _) <- runInteractiveCommand ("cabal test " ++ unwords args)
    liftM (not . any isTestFailure) (readLines out)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        options -> do
            result <- runCabalTest options
            if result then exitSuccess else exitFailure
    where
        usage = putStrLn "Usage: run-cabal-test [options]"
