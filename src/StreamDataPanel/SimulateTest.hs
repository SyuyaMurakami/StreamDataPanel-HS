module StreamDataPanel.SimulateTest 
(
randomValueMaybe,
randomStreamMaybe
) where

import System.Random (randomRIO)
import Control.Monad (forM)

import StreamDataPanel.StreamData (StreamValue(..), Stream, packValue)

randomValueMaybe :: String -> IO (Maybe StreamValue)
randomValueMaybe chartType
    | chartType `elem` ["sequence", "line", "bar"] = do
        value <- randomRIO (50.0, 150.0) :: IO Double
        return . Just . NumberValue $ value

    | chartType `elem` ["sequences", "lines", "bars"] = do
        value1 <- randomRIO (50.0, 150.0) :: IO Double
        value2 <- randomRIO (30.0, 130.0) :: IO Double
        return . Just . DimensionValue $ (["A", "B"], [value1, value2])

    | chartType == "scatter" = do
        value1 <- randomRIO (50.0, 150.0) :: IO Double
        value2 <- randomRIO (30.0, 130.0) :: IO Double
        return . Just . CoordinateValue $ (value1, value2)

    | chartType `elem` ["area", "pie"] = do
        let dimension = ["A", "B", "C", "D", "E", "F", "G"]
        value <- forM dimension $ \_ -> (randomRIO (50.0, 150.0) :: IO Double)
        return . Just . DimensionValue $ (dimension, value)

    | chartType == "areas" = do
        let dimensions = ["A", "B", "C", "D", "E", "F", "G"]
        let series = ["X", "Y"]
        valueA <- forM dimensions $ \_ -> (randomRIO (50.0, 150.0) :: IO Double)
        valueB <- forM dimensions $ \_ -> (randomRIO (50.0, 150.0) :: IO Double)
        return . Just . DimensionsValue $ (dimensions, series, [valueA, valueB])

    | chartType == "radar" = do
        let dimension = ["A", "B", "C", "D", "E", "F", "G"]
        let valueMax = map (\_ -> (150.0 :: Double)) dimension
        value <- forM dimension $ \_ -> (randomRIO (50.0, 150.0) :: IO Double)
        return . Just . RadarValue $ (dimension, valueMax, value)

    | chartType == "surface" = do
        let start = -10.0
            stop  = 10.0
            step  = 1.0
            xRange = takeWhile (<= stop - step/2) [start, start + step ..] :: [Double]
            yRange = takeWhile (<= stop - step/2) [start, start + step ..] :: [Double]
            axis = ("moneyness", "dte", "vega")
        value <- forM [(x, y) | x <- xRange, y <- yRange] $ \(x, y) -> do
            randomOffset <- randomRIO (0.0, 50.0) :: IO Double
            let z = 3 * x * x + y + randomOffset
            return (x, y, z)
        let shape = (length xRange, length yRange)
        return . Just . SurfaceValue $ (axis, shape, value)

    | chartType == "text" = do
        value <- randomRIO (0.0, 50.0) :: IO Double
        return . Just . StringValue $ "You have a text: " ++ (show value)

    | chartType == "gauge" = do
        value <- randomRIO (-1.0, 1.0) :: IO Double
        return . Just . GaugeValue $ ("Delta", (-1, 1), value)

    | otherwise = return Nothing

randomStreamMaybe :: String -> IO (Maybe Stream)
randomStreamMaybe chartType = do
    v <- randomValueMaybe chartType
    case v of 
        Just randomValue -> packValue randomValue >>= return . Just 
        Nothing -> return Nothing






