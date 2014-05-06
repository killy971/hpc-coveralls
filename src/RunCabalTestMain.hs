module Main where

import Control.Concurrent.Async (Async, async, waitBoth)
import Control.Monad
import Data.List
import Data.List.Split
import GHC.IO.Handle
import System.IO (hPutStrLn, stdout, stderr)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Process
import Text.Regex.Posix

defaultCabalName :: String
defaultCabalName = "cabal"

isTestFailure :: String -> Bool
isTestFailure line = line =~ "^Test suite .*: FAIL$"

readLines :: Handle -> Handle -> IO [String]
readLines hIn hOut = do
    isEOF <- hIsEOF hIn
    if isEOF
        then return []
        else do
            x <- hGetLine hIn
            hPutStrLn hOut x
            xs <- readLines hIn hOut
            return (x : xs)

checkFailure :: Handle -> Handle -> IO (Async Bool)
checkFailure hIn = async . liftM (not . any isTestFailure) . readLines hIn

runCabalTest :: String -> [String] -> IO Bool
runCabalTest cabalName args = do
    (_, out, err, _) <- runInteractiveCommand (cabalName ++ " test " ++ unwords args)
    aOutResult <- checkFailure out stdout
    aErrResult <- checkFailure err stderr
    results <- waitBoth aOutResult aErrResult
    return $ uncurry (&&) results

getCabalName :: [String] -> Maybe String
getCabalName [] = Just defaultCabalName
getCabalName [arg] = case splitOn "=" arg of
    (_ : cabalName : _) -> Just cabalName
    _ -> Nothing
getCabalName _ = Nothing

main :: IO ()
main = do
    cmdArgs <- getArgs
    case cmdArgs of
        ["--help"] -> usage >> exitSuccess
        ["-h"] -> usage >> exitSuccess
        options -> case mCabalName of
            Just cabalName -> do
                result <- runCabalTest cabalName cabalTestArgs
                if result then exitSuccess else exitFailure
            Nothing -> usage >> exitFailure
            where (runCabalTestArgs, cabalTestArgs) = partition (=~ "^--cabal-name=.*") options
                  mCabalName = getCabalName runCabalTestArgs
    where usage = putStrLn "Usage: run-cabal-test [run-cabal-test-options] [cabal-test-options]"
