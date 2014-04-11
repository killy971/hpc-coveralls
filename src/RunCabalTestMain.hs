module Main where

import Control.Monad
import Data.List
import Data.List.Split
import GHC.IO.Handle
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.Process
import Text.Regex.Posix

defaultCabalName :: String
defaultCabalName = "cabal"

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

runCabalTest :: String -> [String] -> IO Bool
runCabalTest cabalName args = do
    (_, out, err, _) <- runInteractiveCommand (cabalName ++ " test " ++ unwords args)
    outResult <- liftM (not . any isTestFailure) (readLines out)
    errResult <- liftM null (readLines err)
    return $ outResult && errResult

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
