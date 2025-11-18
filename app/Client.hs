module Main where

import Network.Wai (Application, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (StaticSettings,staticApp, defaultFileServerSettings)
import Network.HTTP.Types (status200, hContentType)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Data.Aeson (Value(..), encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import StreamDataPanel.JsonTool (toText, fromText, readJson, getValueMaybe)

import Paths_stream_data_panel (getDataDir, getDataFileName)

readConfig :: IO Value
readConfig = getDataFileName "config/config.json" >>= readJson

parseConfigWarp :: Value -> Maybe (Int, String, String)
parseConfigWarp v = case getValueMaybe v "WARP_CONFIG" of
    Just warpConfig -> case (getValueMaybe warpConfig "PORT", getValueMaybe warpConfig "DIR", getValueMaybe warpConfig "ROUTE") of
            (Just (Number port), Just (String dir), Just (String route)) -> Just (truncate (realToFrac $ port :: Double), fromText dir, fromText route)
            _ -> Nothing
    Nothing -> Nothing

parseConfigApp :: Value -> Maybe Value
parseConfigApp v = getValueMaybe v "APP_CONFIG"

getSetting :: FilePath -> IO StaticSettings
getSetting staticDirName = do
    currentDir <- getCurrentDirectory
    return . defaultFileServerSettings $ currentDir </> staticDirName

configHandler :: BSL.ByteString -> Application
configHandler configJson _req respond = respond $ responseLBS status200 [(hContentType, BS.pack "application/json")] configJson

app :: FilePath -> String -> BSL.ByteString -> Application
app staticDirName route configJson req respond
    | pathInfo req == [toText route] = configHandler configJson req respond
    | otherwise = getSetting staticDirName >>= \settings -> staticApp settings req respond

main :: IO ()
main = do
    putStrLn "--- StreamDataPanel Web Client Running ---"
    config <- readConfig
    dirRoot <- getDataDir
    case (parseConfigApp config, parseConfigWarp config) of
        (Just configJson, Just (port, dirName, route)) -> do
            let dir = dirRoot </> dirName
            putStrLn $ "--- Web client on http://localhost:" ++ show port ++ " ---"
            run port $ app dir route (encode configJson)
        _ -> putStrLn "--- Parse Warp Config Error ---"
