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

module Trace.Hpc.Coveralls.Curl ( postJson, PostResult (..) ) where

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
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
         -> IO PostResult -- ^ POST request result
postJson path url = do
    h <- initialize
    setopt h (CurlVerbose True)
    setopt h (CurlURL url)
    setopt h (CurlHttpPost $ httpPost path)
    r <- perform_with_response_ h
    return $ parseResponse r
