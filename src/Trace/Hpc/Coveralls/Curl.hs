{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Curl
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Coveralls.Curl ( postJson, readCoverageResult, PostResult (..) ) where

import           Control.Applicative
import           Control.Monad
import           Control.Retry
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List.Split
import           Data.Maybe
import           Network.Curl
import           Safe
import           Trace.Hpc.Coveralls.Types

parseResponse :: CurlResponse -> PostResult
parseResponse r = case mError of
    Just True -> PostFailure $ fromMaybe ("error message not found. " ++ responseDump) mMessage
    _         -> case respCurlCode r of
        CurlOK      -> maybe (PostFailure $ "no url found. " ++ responseDump) PostSuccess mUrl
        _           -> PostFailure $ "curl failure. " ++ responseDump
    where mUrl     = mGetField "url"
          mMessage = mGetField "message"
          mError   = mGetField "error"
          mGetField fieldName = do
              result <- decode $ LBS.pack (respBody r)
              parseMaybe (.: fieldName) result
          responseDump = "CurlCode: " ++ (show $ respCurlCode r) ++ ", Body: " ++ (show $ respBody r)

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

-- | Exponential retry policy of 10 seconds initial delay, up to 5 times
expRetryPolicy :: RetryPolicy
expRetryPolicy = exponentialBackoff tenSecondsInMicroSeconds <> limitRetries 3
    where tenSecondsInMicroSeconds = 10 * 1000 * 1000

performWithRetry :: IO (Maybe a) -> IO (Maybe a)
performWithRetry = retrying expRetryPolicy isNothingM
    where isNothingM _ = return . isNothing

-- | Extract the total coverage percentage value from coveralls coverage result
--   page content.
--   The current implementation is kept as low level as possible in order not
--   to increase the library build time, by not relying on additional packages.
extractCoverage :: String -> Maybe String
extractCoverage body = splitOn "<" <$> splitOn prefix body `atMay` 1 >>= headMay
    where prefix = "div class='run-statistics'>\n<strong>"

-- | Read the coveraege result page from coveralls.io
readCoverageResult :: URLString         -- ^ target url
                   -> Bool              -- ^ print json response if true
                   -> IO (Maybe String) -- ^ coverage result
readCoverageResult url printResponse =
    performWithRetry readAction
    where readAction = do
              response <- curlGetString url curlOptions
              when printResponse $ putStrLn $ snd response
              return $ case response of
                  (CurlOK, body) -> extractCoverage body
                  _ -> Nothing
              where curlOptions = [
                        CurlTimeout 60,
                        CurlConnectTimeout 60]
