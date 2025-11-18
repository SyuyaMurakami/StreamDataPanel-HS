{-# LANGUAGE ScopedTypeVariables #-}
module StreamDataPanel 
(
DataMap,
DataValue,
start,
with,
new,
add,
delete,
update,
fresh,
random,
) where

import Data.Aeson (Value(..))
import Control.Concurrent (ThreadId, killThread, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Network.WebSockets (ServerApp, Connection, runServer, acceptRequest, requestPath, pendingRequest, receiveDataMessage) 

import StreamDataPanel.JsonTool (getValueMaybe, fromText, toByteStringStrict)
import StreamDataPanel.StreamData (Streamable, Stream, toStream)
import StreamDataPanel.SimulateTest (randomStreamMaybe)
import StreamDataPanel.QueueServer (DataMap, DataKey, DataValue, DataConnection, newDataMap, lookupDataValue, addConnection, getDataCache, deleteConnection, updateDataCacheSTM, getDataConnectionsSTM, addDataValue, deleteDataValue)
import StreamDataPanel.SocketConnection (sendTo, recvFrom, routeFailure, closeConnection, closeOnInvalidPath, closeOnInvalidSubscriptionFormat, subscribeFailure, subscribeSuccess)

with :: String -> Int -> String -> (DataMap -> IO ()) -> IO ()
with host port route func = do
    (dataMap, serverThread) <- start host port route
    func dataMap
    killThread serverThread

start :: String -> Int -> String -> IO (DataMap, ThreadId)
start host port route = do
    dataMap <- newDataMap
    serverThread <- forkIO $ runServer host port $ server dataMap route
    return (dataMap, serverThread)

server :: DataMap -> String -> ServerApp
server dataMap route pending = do
    conn <- acceptRequest pending
    let targetPath = requestPath . pendingRequest $ pending
    if targetPath == toByteStringStrict route
        then subscribe dataMap conn
        else do
            sendTo conn routeFailure
            closeConnection conn 1008 closeOnInvalidPath

subscribe :: DataMap -> Connection -> IO ()
subscribe dataMap conn = do
    subscriptionMessage <- recvFrom conn :: IO (Maybe Value) 
    case subscriptionMessage of
        Just msg -> do
            case resolve msg of
                Just dataKey -> do
                    dataValueFind <- lookupDataValue dataMap dataKey
                    case dataValueFind of
                        Just dataValue -> register conn dataValue >>= wait conn dataValue 
                        Nothing -> sendTo conn subscribeFailure >> closeConnection conn 1008 closeOnInvalidSubscriptionFormat
                Nothing -> return ()
        Nothing -> return ()

resolve :: Value -> Maybe DataKey
resolve v = case (getValueMaybe v "chart_type", getValueMaybe v "key_word") of
        (Just (String ct), Just (String kw)) -> Just (fromText ct, fromText kw)
        _ -> Nothing

register :: Connection -> DataValue -> IO DataConnection
register conn dataValue = do
    dataConnection <- addConnection dataValue conn
    cache <- getDataCache dataValue
    sendTo conn subscribeSuccess
    sendTo conn cache
    return dataConnection

wait :: Connection -> DataValue -> DataConnection -> IO ()
wait conn dataValue dataConnection = do
    let connectionListener = forever $ do
            _ <- receiveDataMessage conn 
            return ()
    catch connectionListener (\(_::SomeException) -> do
        deleteConnection dataValue dataConnection
        )

new :: DataMap -> DataKey -> Stream -> IO DataValue
new = addDataValue

add :: (Streamable a) => DataMap -> DataKey -> a -> IO DataValue
add dataMap dataKey value = toStream value >>= addDataValue dataMap dataKey 

delete :: DataMap -> DataKey -> IO ()
delete = deleteDataValue

update :: DataValue -> Stream -> IO ()
update dataValue s = do
    conns <- atomically $ do
        updateDataCacheSTM dataValue s
        getDataConnectionsSTM dataValue
    mapM_ (\conn -> sendTo conn s) conns

fresh :: (Streamable a) => DataValue -> a -> IO ()
fresh dataValue v = toStream v >>= update dataValue

random :: String -> IO (Maybe Stream)
random = randomStreamMaybe


