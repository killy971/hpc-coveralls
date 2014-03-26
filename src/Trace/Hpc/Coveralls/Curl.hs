{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Trace.Hpc.Coveralls.Curl (postJson) where

import Network.Curl

httpPost :: String -> [HttpPost]
httpPost path = [HttpPost "json_file" Nothing (ContentFile path) [] Nothing]

showResponse :: CurlResponse -> String
showResponse r = show (respCurlCode r) ++ show (respBody r)

postJson :: String -> URLString -> IO String
postJson path url = do
    h <- initialize
    setopt h (CurlVerbose True)
    setopt h (CurlURL url)
    setopt h (CurlHttpPost $ httpPost path)
    r <- perform_with_response_ h
    return $ showResponse r
