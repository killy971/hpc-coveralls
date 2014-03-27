{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Curl
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Coveralls.Curl (postJson) where

import Network.Curl

httpPost :: String -> [HttpPost]
httpPost path = [HttpPost "json_file" Nothing (ContentFile path) [] Nothing]

showResponse :: CurlResponse -> String
showResponse r = show (respCurlCode r) ++ show (respBody r)

-- | Send file content over HTTP using POST request
postJson :: String -> URLString -> IO String
postJson path url = do
    h <- initialize
    setopt h (CurlVerbose True)
    setopt h (CurlURL url)
    setopt h (CurlHttpPost $ httpPost path)
    r <- perform_with_response_ h
    return $ showResponse r
