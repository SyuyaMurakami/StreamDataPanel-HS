module StreamDataPanel.QueueServer (
  DataConnection(..),
  DataCache,
  DataConnections,
  DataValue,
  DataKey,
  DataMap,
  newDataMap,
  printDataMap,
  lookupDataValue,
  getDataValue,
  addDataValue,
  deleteDataValue,
  getDataCache,
  printDataCache,
  updateDataCacheSTM,
  updateDataCache,
  newDataConnection,
  getDataConnectionsSTM,
  getDataConnections,
  addConnection,
  deleteConnection,
  getDataConnectionSize
) where

import Control.Concurrent.STM (TVar, STM, newTVar, readTVar, writeTVar, atomically)
import Data.Hashable (Hashable(..))
import Data.Unique (Unique, newUnique, hashUnique)
import Data.Maybe (fromJust)
import Network.WebSockets (Connection)
import Control.Monad (forM)
import ListT (toList)
import qualified StmContainers.Map as STMMap (Map, new, listT, lookup, insert, delete)
import qualified StmContainers.Set as STMSet (Set, new, listT, insert, delete, size)

import StreamDataPanel.StreamData (Stream)

type CharType = String
type KeyWord  = String
type DataKey = (CharType, KeyWord)

newtype DataConnection = DataConnection (Unique, Connection)

instance Eq DataConnection where
  (DataConnection (u1, _)) == (DataConnection (u2, _)) = u1 == u2

instance Ord DataConnection where
  compare (DataConnection (u1, _)) (DataConnection (u2, _)) = compare u1 u2

instance Hashable DataConnection where
  hashWithSalt s (DataConnection (u, _)) = s `hashWithSalt` (hashUnique u)

type DataCache = TVar Stream
type DataConnections = STMSet.Set DataConnection
type DataValue = (DataCache, DataConnections)
type DataMap = STMMap.Map DataKey DataValue

newDataMap :: IO DataMap
newDataMap = atomically STMMap.new

printDataMap :: DataMap -> IO ()
printDataMap dataMap = do
  results <- atomically $ do
    let listTStream = STMMap.listT dataMap
    kvs <- toList listTStream
    forM kvs $ \((chartType, keyWord), (_, dataConns)) -> do
      count <- STMSet.size dataConns
      return ("Data Key: (" ++ chartType ++ ", " ++ keyWord ++ "), ", "Connection Num: " ++ (show count))
  print results

lookupDataValueSTM :: DataMap -> DataKey -> STM (Maybe DataValue)
lookupDataValueSTM dataMap dataKey = STMMap.lookup dataKey dataMap

lookupDataValue :: DataMap -> DataKey -> IO (Maybe DataValue)
lookupDataValue dataMap dataKey = atomically $ lookupDataValueSTM dataMap dataKey

getDataValueSTM :: DataMap -> DataKey -> STM DataValue
getDataValueSTM dataMap dataKey = lookupDataValueSTM dataMap dataKey >>= return . fromJust

getDataValue :: DataMap -> DataKey -> IO DataValue
getDataValue dataMap dataKey = atomically $ getDataValueSTM dataMap dataKey

addDataValue :: DataMap -> DataKey -> Stream -> IO DataValue
addDataValue dataMap dataKey initValue = do
  res <- lookupDataValue dataMap dataKey
  case res of
    Just v -> return v
    Nothing -> atomically $ do
      cache <- newTVar initValue
      conns <- STMSet.new
      STMMap.insert (cache, conns) dataKey dataMap
      return (cache, conns)

deleteDataValue :: DataMap -> DataKey -> IO ()
deleteDataValue dataMap dataKey = atomically $ STMMap.delete dataKey dataMap

getDataCacheSTM :: DataValue -> STM Stream
getDataCacheSTM = readTVar . fst

getDataCache :: DataValue -> IO Stream
getDataCache dataValue = atomically $ getDataCacheSTM dataValue

printDataCache :: DataValue -> IO ()
printDataCache dataValue = getDataCache dataValue >>= print

updateDataCacheSTM :: DataValue -> Stream -> STM ()
updateDataCacheSTM dataValue stream = writeTVar (fst dataValue) stream

updateDataCache :: DataValue -> Stream -> IO ()
updateDataCache dataValue stream = atomically $ updateDataCacheSTM dataValue stream

newDataConnection :: Connection -> IO DataConnection
newDataConnection conn = newUnique >>= \u -> return (DataConnection (u, conn))

getDataConnectionsSTM :: DataValue -> STM [Connection]
getDataConnectionsSTM dataValue = do
  let listTConnections = STMSet.listT . snd $ dataValue
  connections <- toList listTConnections
  return $ map (\(DataConnection (_, conn)) -> conn) connections

getDataConnections :: DataValue -> IO [Connection]
getDataConnections dataValue = atomically $ getDataConnectionsSTM dataValue

addConnectionSTM :: DataValue -> Connection -> Unique -> STM DataConnection
addConnectionSTM dataValue conn u = return (DataConnection (u, conn)) >>= \dataConnection -> STMSet.insert dataConnection (snd dataValue) >> return dataConnection

addConnection :: DataValue -> Connection -> IO DataConnection
addConnection dataValue conn = newUnique >>= \u -> atomically $ addConnectionSTM dataValue conn u

deleteConnectionSTM :: DataValue -> DataConnection -> STM ()
deleteConnectionSTM dataValue item = STMSet.delete item (snd dataValue)

deleteConnection :: DataValue -> DataConnection -> IO ()
deleteConnection dataValue item = atomically $ deleteConnectionSTM dataValue item

getDataConnectionSizeSTM :: DataValue -> STM Int
getDataConnectionSizeSTM dataValue = STMSet.size . snd $ dataValue

getDataConnectionSize :: DataValue -> IO Int
getDataConnectionSize dataValue = atomically $ getDataConnectionSizeSTM dataValue
