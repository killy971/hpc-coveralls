-- |
-- Module:      Trace.Hpc.Coveralls.Cabal
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for reading cabal package name and version.

module Trace.Hpc.Coveralls.Cabal (currDirPkgNameVer, getPackageNameVersion) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List (intercalate, isSuffixOf)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import System.Directory

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile dir = do
    files <- (filter isCabal <$> getDirectoryContents dir) >>= filterM doesFileExist
    case files of
        [file] -> return $ Just file
        _ -> return Nothing
    where isCabal filename = ".cabal" `isSuffixOf` filename && length filename > 6

getPackageNameVersion :: FilePath -> IO (Maybe String)
getPackageNameVersion file = do
    orig <- readFile file
    case parsePackageDescription orig of
        ParseFailed _ -> return Nothing
        ParseOk _warnings gpd -> return $ Just $ name ++ "-" ++ version
            where pkg = package . packageDescription $ gpd
                  PackageName name = pkgName pkg
                  version = showVersion (pkgVersion pkg)
                  showVersion = intercalate "." . map show . versionBranch

currDirPkgNameVer :: IO (Maybe String)
currDirPkgNameVer = runMaybeT $ pkgNameVersion currentDir
    where pkgNameVersion = MaybeT . getPackageNameVersion <=< MaybeT . getCabalFile
          currentDir = "."
