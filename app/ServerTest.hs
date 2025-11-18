module Main where

import Data.Aeson (Value(..))
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (forever, forM, forM_)

import StreamDataPanel (DataMap, DataValue, random, update, new, with)
import StreamDataPanel.JsonTool (fromText, readJson, getValueMaybe)

import Paths_stream_data_panel (getDataFileName)

readConfig :: IO Value
readConfig = getDataFileName "config/config.json" >>= readJson

parseConfig :: Value -> Maybe (String, Int, String)
parseConfig v = case getValueMaybe v "WEBSOCKET_CONFIG" of
    Just websocket -> case (getValueMaybe websocket "HOST", getValueMaybe websocket "PORT", getValueMaybe websocket "ROUTE") of
        (Just (String host), Just (Number port), Just (String route)) -> Just (fromText host, truncate (realToFrac port :: Double), fromText route)
        _ -> Nothing
    Nothing -> Nothing

simulate :: String -> DataValue -> IO ()
simulate chartType dataValue = forever $ do
    mv <- random chartType
    case mv of
        Just v -> dataValue `update` v
        Nothing -> return ()
    threadDelay 10000

simulateAll :: DataMap -> IO ()
simulateAll dataMap = do
    threadIdList <- forM chart $ \chartType -> do
        mv <- random chartType
        case mv of 
            Just v -> do
                dataValue <- new dataMap (chartType,"test") v
                dataThread <- forkIO $ simulate chartType dataValue
                return . Just $ dataThread
            Nothing -> return Nothing
    threadDelay 600000000
    forM_ threadIdList $ \dataThread -> case dataThread of
        Just tid -> killThread tid
        Nothing -> putStrLn "--- Simulate Sub-Thread Start Failed ---"

chart :: [String]
chart = ["sequence", "line", "bar", "sequences", "lines", "bars", "scatter", "area", "areas", "pie", "radar", "surface", "text", "gauge"]

main :: IO ()
main = do
    putStrLn "--- StreamDataPanel Server Test Begin ---"
    putStrLn "--- This Service Will Last 10 Minutes And Shut Down Automatically ---"
    config <- readConfig
    case parseConfig config of
        Just (host, port, route) -> do
            putStrLn $ "--- Websocket service on ws://" ++ host ++ ":" ++ show port ++ "/" ++ route ++ " ---"
            with host port route simulateAll
            putStrLn "--- StreamDataPanel Server Test Finished ---"
        Nothing -> putStrLn "--- Parse Websocket Config Error ---"


