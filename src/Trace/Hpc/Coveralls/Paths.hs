{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Paths
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants and functions for hpc coverage report output.

module Trace.Hpc.Coveralls.Paths where

import           Control.Applicative
import           Control.Monad
import           Data.List                  (nub)
import           Data.Maybe                 (fromMaybe)
import           Data.Traversable           (traverse)
import           System.Directory           (doesDirectoryExist,
                                             getDirectoryContents)
import           System.Directory.Tree      (AnchoredDirTree (..), dirTree,
                                             readDirectoryWith)
import           System.FilePath
import           Trace.Hpc.Coveralls.Config
import           Trace.Hpc.Coveralls.Stack
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Tix

data HpcDir = HpcDir
  { tixDir        :: !FilePath
  , mixDir        :: !FilePath
  } deriving (Show, Eq)

getHpcDir :: Config -> IO (Either String HpcDir)
getHpcDir config
    | useStackCov config = stackHpcDir
getHpcDir config = buildHpcDir (tixDirPath config) (mixDirPath config)

cabalDistDir :: FilePath
cabalDistDir = "dist/"

cabalHpcDirs :: [FilePath]
cabalHpcDirs = map (cabalDistDir ++) ["hpc/vanilla/", "hpc/"]

stackHpcDir :: IO (Either String HpcDir)
stackHpcDir = do
    canExecStack <- checkStackVersion
    canGetStackHpc <- whenM canExecStack (Left "cannot exec `stack`") $ do
        isStackProject <- checkStackProject
        return $ if isStackProject
            then Right ()
            else Left "this is not a stack project"

    case canGetStackHpc of
        Right _  -> Right <$> getStackHpcDir
        Left msg -> return $ Left msg

getStackHpcDir :: IO HpcDir
getStackHpcDir = HpcDir
    <$> ((</>) <$> stackHpcRootPath <*> stackProjectName)
    <*> stackDistHpcRootPath

buildHpcDir :: Maybe FilePath -> Maybe FilePath -> IO (Either String HpcDir)
buildHpcDir (Just tixD) (Just mixD) = return . Right $ HpcDir tixD mixD
buildHpcDir mtixD mmixD = do
    mPath <- firstExistingDirectory cabalHpcDirs
    mhpcDir <- case mPath of
        Just hpcDir -> return . Right $ HpcDir
          { tixDir        = hpcDir </> "tix"
          , mixDir        = hpcDir </> "mix"
          }
        Nothing     -> do
            mhpcDir <- stackHpcDir
            case mhpcDir of
                Right _ -> return mhpcDir
                Left  _ -> return . Left $ "not found either cabal or stack hpc directory"

    case mhpcDir of
        Left _       -> return mhpcDir
        Right hpcDir -> return . Right $ HpcDir
          { tixDir        = fromMaybe (tixDir hpcDir) mtixD
          , mixDir        = fromMaybe (mixDir hpcDir) mmixD
          }

getMixDirPaths :: Maybe String -- ^ target package name-version
               -> HpcDir       -- ^ hpc output base directory
               -> String       -- ^ test suite name
               -> TixModule    -- ^ tix module
               -> [FilePath]   -- ^ mix file paths
getMixDirPaths mPkgNameVer hpcDir testSuiteName tix = nub $ do
    dirName <- dirs
    return $ mixDir hpcDir </> dirName
    where
        dirs = case span (/= '/') modName of
              (_, [])        -> [ testSuiteName ]
              (packageId, _) -> [ "", packageId ] ++ maybe [] pure mPkgNameVer

        TixModule modName _ _ _ = tix

getTixFilePath :: HpcDir -> String -> FilePath
getTixFilePath hpcDir testSuiteName
    =   tixDir hpcDir
    </> testSuiteName
    </> getTixFileName testSuiteName

firstExistingDirectory :: [FilePath] -> IO (Maybe FilePath)
firstExistingDirectory = fmap msum . mapM pathIfExist
    where pathIfExist path = do
              pathExists <- doesDirectoryExist path
              return $ if pathExists then Just path else Nothing

dumpDirectory :: FilePath -> IO ()
dumpDirectory path = do
    directoryExists <- doesDirectoryExist path
    unless directoryExists $ putStrLn ("Couldn't find the directory " ++ path)
    putStrLn ("Dumping " ++ path ++ " directory content:")
    contents <- getDirectoryContents path
    traverse putStrLn contents
    return ()

dumpDirectoryTree :: FilePath -> IO ()
dumpDirectoryTree path = do
    putStrLn ("Dumping " ++ path ++ " directory tree:")
    tree <- readDirectoryWith return path
    traverse putStrLn $ dirTree tree
    return ()
