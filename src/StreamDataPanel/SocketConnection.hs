module StreamDataPanel.SocketConnection 
(
closeOnInvalidPath,
closeOnInvalidSubscriptionFormat,
closeConnection,
routeFailure,
subscribeFailure,
subscribeSuccess,
sendTo,
recvFrom
) where

import Data.Word (Word16)
import Data.Aeson (FromJSON, ToJSON, Value, object, encode, decode, (.=))
import Network.WebSockets (Connection, receiveData, sendCloseCode, sendTextData)

import StreamDataPanel.JsonTool (toKey, toText, toByteStringStrict)

closeOnInvalidPath :: String
closeOnInvalidPath = "Connection attempt on invalid path."

closeOnInvalidSubscriptionFormat :: String
closeOnInvalidSubscriptionFormat = "Invalid subscription format."

closeConnection :: Connection -> Word16 -> String -> IO ()
closeConnection conn code reason = sendCloseCode conn code (toByteStringStrict reason)

routeFailure :: Value
routeFailure = object 
    [ toKey "status" .= toText "failure"
    , toKey "message" .= toText "Connection attempt on invalid path."
    ]

subscribeFailure :: Value
subscribeFailure = object
    [ toKey "status"  .= toText "failure" 
    , toKey "message" .= toText "Invalid chart type or keyword (not registered)."
    ]

subscribeSuccess :: Value
subscribeSuccess = object
    [ toKey "status" .= toText "success"
    , toKey "message" .= toText "Subscription successful."
    ]

sendTo :: (ToJSON a) => Connection -> a -> IO ()
sendTo conn jsonable = sendTextData conn $ encode jsonable

recvFrom :: (FromJSON a) => Connection -> IO (Maybe a)
recvFrom conn = receiveData conn >>= return . decode





