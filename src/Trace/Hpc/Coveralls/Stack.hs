module Trace.Hpc.Coveralls.Stack
    ( checkStackVersion
    , checkStackProject
    , stackProjectName
    , stackPath
    , stackHpcRootPath
    , stackDistHpcRootPath
    ) where

import           Control.Applicative
import           Control.Exception
import           Data.List                (isPrefixOf)
import           System.FilePath
import           System.IO.Error
import           System.Process           (callProcess, readProcess)
import           Trace.Hpc.Coveralls.Util

checkStackVersion :: IO Bool
checkStackVersion = handle (handleIOError False) $ do
    callProcess "stack" ["--version"]
    return True
    where
        handleIOError :: a -> IOError -> IO a
        handleIOError d e
            | isDoesNotExistError e = return d
        handleIOError _ e = throwIO e

checkStackProject :: IO Bool
checkStackProject = do
     projectRoot <- stackPath "project-root"
     stackRoot   <- stackPath "stack-root"
     return . not $ isPrefixOf stackRoot projectRoot

stackPath :: String -> IO FilePath
stackPath key = stripString <$> readProcess "stack" ["path", "--" ++ key] ""

stackProjectName :: IO String
stackProjectName = takeFileName <$> stackPath "project-root"

stackHpcRootPath :: IO FilePath
stackHpcRootPath = stackPath "local-hpc-root"

stackDistDirPath :: IO FilePath
stackDistDirPath = stackPath "dist-dir"

stackDistHpcRootPath :: IO FilePath
stackDistHpcRootPath = (</> "hpc") <$> stackDistDirPath
