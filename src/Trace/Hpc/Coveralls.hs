module Trace.Hpc.Coveralls where

import Trace.Hpc.Tix

-- dist/hpc/tix

tixToJson :: Tix -> String
tixToJson _ = "TODO"

generateCoverallsFromTix :: String -> IO ()
generateCoverallsFromTix name = do
    mtix <- readTix $ "dist/hpc/tix/" ++ name ++ "/" ++ (getTixFileName name)
    case mtix of
        Nothing -> return ()
        Just tix -> putStrLn $ tixToJson tix
