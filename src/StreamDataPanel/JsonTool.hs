module StreamDataPanel.JsonTool 
(
listDirectory,
readString,
ensureDirectory,
writeString,
toText,
fromText,
toKey,
fromKey,
toByteStringStrict,
fromByteStringStrict,
toByteString,
fromByteString,
toByteStringLazy,
fromByteStringLazy,
toJsonMaybe,
toJson,
fromJson,
readJsonMaybe,
readJson,
writeJson,
getValueMaybe,
updateValueMaybe,
toValueString,
toValueNumber,
toValueBool,
) where

import Data.Aeson (FromJSON, ToJSON, Value(..), encode, decode)
import Data.Aeson.KeyMap as KM ((!?), insert)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import System.IO (withFile, utf8, IOMode (ReadMode, WriteMode), openFile, hSetEncoding, hGetContents, hClose, hPutStr)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath (takeDirectory)
import Control.DeepSeq (deepseq)
import qualified Data.Aeson.Key as K (Key, fromText, toText)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, readFile, writeFile, fromStrict, toStrict)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = getDirectoryContents

readString :: String -> IO String
readString filePath = do
  inputHandle <- openFile filePath ReadMode
  hSetEncoding inputHandle utf8
  x <- hGetContents inputHandle
  x `deepseq` hClose inputHandle
  return x

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectoryIfMissing True . takeDirectory

writeString :: FilePath -> String -> IO ()
writeString path content = ensureDirectory path >> withFile path WriteMode (`hPutStr` content)

toText :: String -> T.Text
toText = T.pack

fromText :: T.Text -> String
fromText = T.unpack
   
toKey :: String -> K.Key
toKey = K.fromText . toText

fromKey :: K.Key -> String
fromKey = T.unpack . K.toText

toByteStringStrict :: String -> B.ByteString
toByteStringStrict = TE.encodeUtf8 . T.pack

fromByteStringStrict :: B.ByteString -> String
fromByteStringStrict = T.unpack . TE.decodeUtf8

toByteString :: String -> BS.ByteString
toByteString = BS.pack

fromByteString :: BS.ByteString -> String
fromByteString = BS.unpack

toByteStringLazy :: String -> BSL.ByteString
toByteStringLazy = BSL.fromStrict . TE.encodeUtf8 . T.pack

fromByteStringLazy :: BSL.ByteString -> String
fromByteStringLazy = T.unpack . TE.decodeUtf8 . BSL.toStrict

toJsonMaybe :: (FromJSON a) => String -> Maybe a
toJsonMaybe = decode . toByteStringLazy

toJson :: (FromJSON a) => String -> a
toJson = fromJust . toJsonMaybe

fromJson :: (ToJSON a) => a -> String
fromJson = fromByteStringLazy . encode

readJsonMaybe :: (FromJSON a) => FilePath -> IO (Maybe a)
readJsonMaybe filePath = BSL.readFile filePath >>= return . decode

readJson :: (FromJSON a) => FilePath -> IO a
readJson filePath = readJsonMaybe filePath >>= return . fromJust

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson filePath v = BSL.writeFile filePath $ encode v

getValueMaybe :: Value -> String -> Maybe Value
getValueMaybe (Object obj) k = obj KM.!? (toKey k)
getValueMaybe _ _ = Nothing

updateValueMaybe :: Value -> String -> Value -> Maybe Value
updateValueMaybe (Object obj) k v = Just . Object $ insert (toKey k) v obj
updateValueMaybe _ _ _ = Nothing

toValueString :: String -> Value
toValueString s = String . toText $ s

toValueNumber :: Scientific -> Value
toValueNumber n = Number n

toValueBool :: Bool -> Value
toValueBool b = Bool b


