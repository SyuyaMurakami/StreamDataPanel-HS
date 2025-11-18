module StreamDataPanel.TimeTool 
(
nowTimestamp,
now,
today,
nowStreamCode,
nowStreamTimestamp
) where

import Data.Time.LocalTime 
    ( getZonedTime
    , ZonedTime
    , zonedTimeToLocalTime
    , localDay
    )
import Data.Time.Format
    ( formatTime
    , defaultTimeLocale
    )

nowTimestamp :: IO ZonedTime
nowTimestamp = getZonedTime

now :: IO String
now = nowTimestamp >>= return . show

today :: IO String
today = nowTimestamp >>= return . show . localDay . zonedTimeToLocalTime

nowStreamCode :: IO String
nowStreamCode = do
    ztime <- nowTimestamp
    let formatStr = "%Y%m%d%H%M%S%q" 
    return $ take 20 $ formatTime defaultTimeLocale formatStr ztime

nowStreamTimestamp :: IO String
nowStreamTimestamp = do
    ztime <- nowTimestamp
    let prefixFormat = "%Y-%m-%dT%H:%M:%S"
    let prefixStr = formatTime defaultTimeLocale prefixFormat ztime
    let fullPicoStr = formatTime defaultTimeLocale "%q" ztime
    let microSecondsStr = take 6 fullPicoStr
    return $ prefixStr ++ "." ++ microSecondsStr



