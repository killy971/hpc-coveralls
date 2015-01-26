{-# LANGUAGE OverloadedStrings #-}

module Trace.Hpc.Coveralls.GitInfo (getGitInfo, GitInfo) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Aeson
import Data.List (nubBy)
import Data.Function (on)
import System.Process (readProcess)

data GitInfo = GitInfo { headRef :: Commit
                       , branch  :: String
                       , remotes :: [Remote] }

instance ToJSON GitInfo where
    toJSON i = object [ "head"    .= headRef i
                      , "branch"  .= branch i
                      , "remotes" .= remotes i]

data Commit = Commit { hash           :: String
                     , authorName     :: String
                     , authorEmail    :: String
                     , committerName  :: String
                     , committerEmail :: String
                     , message        :: String }

instance ToJSON Commit where
    toJSON c = object [ "id"              .= hash c
                      , "author_name"     .= authorName c
                      , "author_email"    .= authorEmail c
                      , "committer_name"  .= committerName c
                      , "committer_email" .= committerEmail c
                      , "message"         .= message c ]

data Remote = Remote { name :: String
                     , url  :: String }

instance ToJSON Remote where
    toJSON r = object [ "name" .= name r
                      , "url"  .= url r ]

git :: [String] -> IO String
git args = init <$> readProcess "git" args []  -- init to strip trailing \n

-- | Get information about the Git repo in the current directory.
getGitInfo :: IO GitInfo
getGitInfo = GitInfo <$> headRef <*> branch <*> getRemotes where
    headRef = Commit <$> git ["rev-parse", "HEAD"]
                  <*> git ["log", "-1", "--pretty=%aN"] <*> git ["log", "-1", "--pretty=%aE"]
                  <*> git ["log", "-1", "--pretty=%cN"] <*> git ["log", "-1", "--pretty=%cE"]
                  <*> git ["log", "-1", "--pretty=%s"]
    branch = git ["rev-parse", "--abbrev-ref", "HEAD"]

getRemotes :: IO [Remote]
getRemotes = nubBy ((==) `on` name) <$> parseRemotes <$> git ["remote", "-v"] where
    parseRemotes :: String -> [Remote]
    parseRemotes input = do
        line <- lines input
        let fields = words line
        guard $ length fields >= 2
        return $ Remote (head fields) (fields !! 1)
