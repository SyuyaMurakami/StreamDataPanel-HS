{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module StreamDataPanel.StreamData
(
NumberData,
StringData,
ShapeData,
CoordinateData,
DimensionData,
DimensionsData,
PointData,
AxisData,
SurfaceData,
RangeData,
GaugeData,
RadarData,
Streamable(..),
StreamValue(..),
Stream(..),
packValue,
toStream,
loadStreamValueMaybe,
loadStreamValue,
dumpStreamValue,
loadStreamMaybe,
loadStream,
dumpStream,
) where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, toEncoding, object, pairs, (.:), (.=))
import Data.Aeson.Types (withObject)

import StreamDataPanel.TimeTool (nowStreamCode, nowStreamTimestamp)
import StreamDataPanel.JsonTool (toKey, toJsonMaybe, fromJson)

type NumberData = Double
type StringData = String
type ShapeData = (Int, Int)
type CoordinateData = (NumberData, NumberData)
type DimensionData = ([StringData], [NumberData])
type DimensionsData = ([StringData], [StringData], [[NumberData]])
type PointData = (NumberData, NumberData, NumberData)
type AxisData = (StringData, StringData, StringData)
type SurfaceData = (AxisData, ShapeData, [PointData])
type RangeData = (NumberData, NumberData)
type GaugeData = (StringData, RangeData, NumberData)
type RadarData = ([StringData], [NumberData], [NumberData])


data StreamValue = 
    NumberValue NumberData
  | StringValue StringData
  | CoordinateValue CoordinateData
  | DimensionValue DimensionData
  | DimensionsValue DimensionsData
  | SurfaceValue SurfaceData
  | GaugeValue GaugeData
  | RadarValue RadarData
  deriving Show

instance FromJSON StreamValue where
    parseJSON v = 
        (NumberValue <$> parseJSON v) <|>
        (StringValue <$> parseJSON v) <|>
        (CoordinateValue <$> parseJSON v) <|>
        (DimensionValue <$> parseJSON v) <|>
        (DimensionsValue <$> parseJSON v) <|>
        (SurfaceValue <$> parseJSON v) <|>
        (GaugeValue <$> parseJSON v) <|>
        (RadarValue <$> parseJSON v)

instance ToJSON StreamValue where
    toJSON (NumberValue v)    = toJSON v
    toJSON (StringValue v)    = toJSON v
    toJSON (CoordinateValue v)= toJSON v
    toJSON (DimensionValue v) = toJSON v
    toJSON (DimensionsValue v)= toJSON v
    toJSON (SurfaceValue v)   = toJSON v
    toJSON (GaugeValue v)     = toJSON v
    toJSON (RadarValue v)     = toJSON v

    toEncoding (NumberValue v)    = toEncoding v
    toEncoding (StringValue v)    = toEncoding v
    toEncoding (CoordinateValue v)= toEncoding v
    toEncoding (DimensionValue v) = toEncoding v
    toEncoding (DimensionsValue v)= toEncoding v
    toEncoding (SurfaceValue v)   = toEncoding v
    toEncoding (GaugeValue v)     = toEncoding v
    toEncoding (RadarValue v)     = toEncoding v


class Streamable m where
    toStreamValue :: m -> StreamValue

instance Streamable Double where
    toStreamValue = NumberValue

instance Streamable String where
    toStreamValue = StringValue

instance Streamable (Double, Double) where
    toStreamValue = CoordinateValue

instance Streamable ([String], [Double]) where
    toStreamValue = DimensionValue

instance Streamable ([String], [String], [[Double]]) where
    toStreamValue = DimensionsValue

instance Streamable ((String, String, String), (Int, Int), [(Double, Double, Double)]) where
    toStreamValue = SurfaceValue

instance Streamable (String, (Double, Double), Double) where
    toStreamValue = GaugeValue

instance Streamable ([String], [Double], [Double]) where
    toStreamValue = RadarValue


loadStreamValueMaybe :: String -> Maybe StreamValue
loadStreamValueMaybe = toJsonMaybe

loadStreamValue :: String -> StreamValue
loadStreamValue = fromJust . loadStreamValueMaybe

dumpStreamValue :: StreamValue -> String
dumpStreamValue = fromJson

data Stream = Stream
  { code :: StringData
  , timestamp :: StringData
  , value   :: StreamValue
  } deriving (Show)

instance FromJSON Stream where
    parseJSON = withObject "Stream" $ \v -> do
        x <- v .: toKey "id"          
        y <- v .: toKey "timestamp"
        z <- v .: toKey "value"
        return (Stream { code = x, timestamp = y, value = z })

instance ToJSON Stream where
    toJSON (Stream {code = x, timestamp = y, value = z}) = object
        [ toKey "id" .= x
        , toKey "timestamp" .= y
        , toKey "value" .= z
        ]
    
    toEncoding (Stream {code = x, timestamp = y, value = z}) = pairs $
        toKey "id" .= x <>
        toKey "timestamp" .= y <>
        toKey "value" .= z


packValue :: StreamValue -> IO Stream
packValue v = do
    cd <- nowStreamCode
    ts <- nowStreamTimestamp
    return Stream { code = cd, timestamp = ts, value = v }

toStream :: (Streamable m) => m -> IO Stream
toStream = packValue . toStreamValue

loadStreamMaybe :: String -> Maybe Stream
loadStreamMaybe = toJsonMaybe

loadStream :: String -> Stream
loadStream = fromJust . loadStreamMaybe

dumpStream :: Stream -> String
dumpStream = fromJson











