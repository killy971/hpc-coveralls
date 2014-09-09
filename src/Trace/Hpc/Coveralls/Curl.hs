{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Curl
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Coveralls.Curl ( postJson, readCoverageResult, PostResult (..) ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List.Split
import           Data.Maybe
import           Network.Curl
import           Trace.Hpc.Coveralls.Types

parseResponse :: CurlResponse -> PostResult
parseResponse r = case respCurlCode r of
    CurlOK -> PostSuccess $ getField "url"
    _      -> PostFailure $ getField "message"
    where getField fieldName = fromJust $ mGetField fieldName
          mGetField fieldName = do
              result <- decode $ LBS.pack (respBody r)
              parseMaybe (.: fieldName) result

httpPost :: String -> [HttpPost]
httpPost path = [HttpPost "json_file" Nothing (ContentFile path) [] Nothing]

-- | Send file content over HTTP using POST request
postJson :: String        -- ^ target file
         -> URLString     -- ^ target url
         -> Bool          -- ^ print json response if true
         -> IO PostResult -- ^ POST request result
postJson path url printResponse = do
    h <- initialize
    setopt h (CurlVerbose True)
    setopt h (CurlURL url)
    setopt h (CurlHttpPost $ httpPost path)
    r <- perform_with_response_ h
    when printResponse $ putStrLn $ respBody r
    return $ parseResponse r

-- | Extract the total coverage percentage value from coveralls coverage result
--   page content.
--   The current implementation is kept as low level as possible in order not
--   to increase the library build time, by not relying on additional packages.
extractCoverage :: String -> String
extractCoverage = head . splitOn "<" . (!! 1) . splitOn prefix
    where prefix = "div class='coverage'>\n<strong>"

-- | Read the coveraege result page from coveralls.io
readCoverageResult :: URLString         -- ^ target url
                   -> IO (Maybe String) -- ^ coverage result
readCoverageResult url = do
    response <- curlGetString url []
    return $ case response of
        (CurlOK, body) -> do
            threadDelay (10 * 60 * 1000) -- wait until the page is available
            Just $ extractCoverage body
        _ -> Nothing
