-- |
-- Module:      Trace.Hpc.Coveralls.Util
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Utility functions.

module Trace.Hpc.Coveralls.Util where

import Data.List
import Data.Semigroup ((<>))
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import Trace.Hpc.Coveralls.Config
import Trace.Hpc.Coveralls.Cabal
import Trace.Hpc.Coveralls.Types
import Trace.Hpc.Coveralls.Paths

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

toFirstAndRest :: (a, b, c, d) -> (a, (b, c, d))
toFirstAndRest (a, b, c, d) = (a, (b, c, d))

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs

mcons :: Maybe a -> [a] -> [a]
mcons Nothing xs = xs
mcons (Just x) xs = x : xs

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (`isPrefixOf` fileName) patterns

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f (x : xs) = f x : xs
mapFirst _ []       = []
 
mapLast :: (a -> a) -> [a] -> [a]
mapLast f [x]      = [f x]
mapLast f (x : xs) = x : mapLast f xs
mapLast _ []       = []

subSeq :: Int -> Int -> [a] -> [a]
subSeq start end = drop start . take end

subSubSeq :: Int -> Int -> [[a]] -> [[a]]
subSubSeq start end = mapFirst (drop start) . mapLast (take end)

groupByIndex :: Int -> [(Int, a)] -> [[a]]
groupByIndex size = take size . flip (++) (repeat []) . groupByIndex' 0 []
    where groupByIndex' _ ys [] = [ys]
          groupByIndex' i ys xx@((xi, x) : xs) = if xi == i
              then groupByIndex' i (x : ys) xs
              else ys : groupByIndex' (i + 1) [] xx

foldFor
  :: (Monoid m, Foldable t)
  => t a
  -> (a -> m)
  -> m
foldFor = (flip foldMap)

findHpcDataDirs :: Config -> IO [FilePath]
findHpcDataDirs config = do
  case hpcDirOverrides config of
    [] -> do
      mHpcDir <- firstExistingDirectory hpcDistDirs 
      case mHpcDir of
        Nothing     -> putStrLn "Couldn't find the hpc data directory" >> dumpDirectory distDir >> ioFailure
        Just hpcDir -> pure [hpcDir]
    potentialHpcDirs -> do
      foldFor potentialHpcDirs $ \potentialHpcDir -> do
        let hpcDir = potentialHpcDir <> "/"
        doesExist <- doesDirectoryExist hpcDir
        if doesExist == False
          then putStrLn ("The hpc data directory override provided does not exist: " <> hpcDir) >> ioFailure
          else pure [hpcDir]

findPackages :: Config -> IO [Package]
findPackages config =
  let
    currDir = "./"

    findPkgRequest :: FindPackageRequest
    findPkgRequest =
      case cabalFile config of
        Just cabalFilePath ->
          useExplicitCabalFiles [(currDir, Just cabalFilePath)]
        Nothing            ->
          let
            packageDirOverrides :: [FilePath]
            packageDirOverrides = packageDirs config
            packageDirs' :: [FilePath]
            packageDirs' =
              if length packageDirOverrides == 0
                then [currDir]
                else packageDirOverrides
          in
            searchTheseDirectories packageDirs'

    renderFindPackageRequestError :: FindPackageRequest -> String
    renderFindPackageRequestError request =
      let
        render :: (FilePath, Maybe FilePath) -> String
        render (_, Just cabalFilePath) = "\nAt location '" <> cabalFilePath <> "'"
        render (dir, Nothing)          = "\nIn directory '" <> dir <> "'"

        indent :: String -> String
        indent = (" " <>)
      in "Couldn't find cabal file..." <> foldMap (indent . render) request

  in do
    pkgs <- getPackages findPkgRequest
    case pkgs of
      [] -> putStrLn (renderFindPackageRequestError findPkgRequest) >> ioFailure
      ps -> pure ps

findTestSuiteNames :: Config -> [Package] -> IO [String]
findTestSuiteNames config pkgs = do
  case testSuites config of
    [] -> do
      let cabalFiles = pkgCabalFilePath <$> pkgs
      foldMap readTestSuiteNames cabalFiles
    testSuiteNames -> pure testSuiteNames

ioFailure :: IO a
ioFailure = putStrLn ("You can get support at " ++ gitterUrl) >> exitFailure
    where gitterUrl = "https://gitter.im/guillaume-nargeot/hpc-coveralls" :: String
