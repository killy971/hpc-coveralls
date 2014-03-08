module Trace.Hpc.Coveralls where

import Trace.Hpc.Tix

-- dist/hpc/tix

tixToJson :: Tix -> String
tixToJson _ = "TODO"

generateCoverallsFromTix :: String -> IO ()
generateCoverallsFromTix name = do
    mtix <- readTix path
    case mtix of
        Nothing -> error $ "Couldn't find the file " ++ path
        Just tix -> putStrLn $ tixToJson tix
    where path = "dist/hpc/tix/" ++ name ++ "/" ++ (getTixFileName name)
