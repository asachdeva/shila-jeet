{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TemplateHaskell, QuasiQuotes, DeriveGeneric, BangPatterns #-}
module FibonacciTimescaleHvega where

import Control.Monad.IO.Class (liftIO)
import Data.Massiv.Array as A
import Streamly.Prelude as S
import Data.Time
import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)
import Control.Concurrent (forkIO)
import Graphics.Vega.VegaLite
import Data.List (sortBy)
import Data.Ord (comparing)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger (runStderrLoggingT)
import GHC.Generics
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Persistent schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PricePoint
  time UTCTime
  price Double
  deriving Show Generic
|]

instance ToJSON PricePoint
instance FromJSON PricePoint

-- API data types
data FibResponse = FibResponse
  { fibLevels :: [(Double, Double)]
  , inGoldenPocket :: Bool
  , supportResistance :: [(Double, Double)]
  } deriving (Show, Eq, Generic)

instance ToJSON FibResponse where
  toJSON (FibResponse levels inPocket sr) = object
    [ "fibonacciLevels" .= levels
    , "inGoldenPocket" .= inPocket
    , "supportResistance" .= sr
    ]

-- Fibonacci retracement levels
fibLevels :: [Double]
fibLevels = [0.236, 0.382, 0.5, 0.618, 0.65, 0.786]

-- Calculate Fibonacci retracement levels
calcFibRetracement :: Double -> Double -> Array U Ix1 Double
calcFibRetracement high low = computeAs Par $ A.fromList Seq fibLevels * pure (high - low) + pure low

-- Find swing high and low
findSwingPoints :: [Double] -> (Double, Double)
findSwingPoints prices = (maximum prices, minimum prices)

-- Check if price is in golden pocket (61.8%–65%)
inGoldenPocket :: Double -> Array U Ix1 Double -> Bool
inGoldenPocket price fibs = let [gpStart, gpEnd] = A.toList $ A.slice' (A.Sz (2 :: Int)) 3 fibs
                            in price >= gpEnd && price <= gpStart

-- Optimized support and resistance with parallel massiv
calcSupportResistance :: [Double] -> [(Double, Double)]
calcSupportResistance prices = do
  let !priceRange = maximum prices - minimum prices
      !binSize = priceRange * 0.005
      !minPrice = minimum prices
      !numBins = ceiling (priceRange / binSize) + 1 :: Int
      !bins = computeAs Par $ makeArrayR D Seq (Sz numBins) (const 0) :: Array U Ix1 Int
      !binIndices = prices `using` parList rdeepseq
      !binArray = A.accumulate_ (+) bins $ map (\p -> (fromIntegral $ floor ((p - minPrice) / binSize), 1)) binIndices
      !significantBins = V.toList $ V.filter (\(i, c) -> c >= 5) $ V.indexed $ A.toVector binArray
      bands = significantBins `using` parList rdeepseq >>= \(i, _) ->
                let binStart = minPrice + fromIntegral i * binSize
                in [(binStart, binStart + binSize)]
  sortBy (comparing fst) bands

-- Vega-Lite chart specification
plotChart :: [(UTCTime, Double)] -> Array U Ix1 Double -> [(Double, Double)] -> VegaLite
plotChart priceData fibLevels srLevels =
  let priceDataJson = toVegaLite
        [ dataFromRows []
          . dataRow (map (\(t, p) -> ("time", VDateTime (toVegaLiteDateTime t)) . ("price", VNumber p)) priceData)
          $ []
        , mark Line []
        , encoding
          . position X [PName "time", PmType Temporal]
          . position Y [PName "price", PmType Quantitative, PTitle "Price (USD)"]
          $ []
        ]
      fibDataJson = toVegaLite
        [ dataFromRows []
          . dataRow (map (\f -> ("level", VNumber f)) (A.toList fibLevels))
          $ []
        , mark Rule []
        , encoding
          . position Y [PName "level", PmType Quantitative]
          . color [MString "#FF0000"]
          $ []
        ]
      srDataJson = toVegaLite
        [ dataFromRows []
          . dataRow (map (\(minP, maxP) -> ("minPrice", VNumber minP) . ("maxPrice", VNumber maxP)) srLevels)
          $ []
        , mark Rect [MOpacity 0.2]
        , encoding
          . position Y [PName "minPrice", PmType Quantitative]
          . position Y2 [PName "maxPrice"]
          . color [MString "#00FF00"]
          $ []
        ]
      config = configure
        . configuration (TitleStyle [TText "Price, Fibonacci, and Support/Resistance Levels"])
        . configuration (Axis [AxisTitle "Time"])
        $ []
  in toVegaLite [width 800, height 400, config, layer [priceDataJson, fibDataJson, srDataJson]]

-- Convert UTCTime to Vega-Lite DateTime
toVegaLiteDateTime :: UTCTime -> DateTime
toVegaLiteDateTime t =
  let (year, month, day) = toGregorian $ utctDay t
      TimeOfDay hour minute second = timeOfDay $ utctDayTime t
  in DateTime [DTYear year, DTMonthNum month, DTDate day, DTHours hour, DTMinutes minute, DTSeconds second]

-- TimescaleDB operations
upsertPrice :: ConnectionPool -> PricePoint -> IO ()
upsertPrice pool price = runSqlPool (insert $ PricePoint (ppTime price) (ppPrice price)) pool

queryPrices :: ConnectionPool -> Int -> IO [(UTCTime, Double)]
queryPrices pool limit = do
  prices <- runSqlPool (selectList [] [LimitTo limit, Desc PricePointTime]) pool
  return $ map (\(Entity _ p) -> (pricePointTime p, pricePointPrice p)) prices

-- Streamly processing for price data
processPriceStream :: PricePoint -> S.SerialT IO PricePoint
processPriceStream price = do
  let adjustedPrice = ppPrice price * 1.01
      isValid = adjustedPrice >= 10000 && adjustedPrice <= 100000
  if isValid then pure $ price { ppPrice = adjustedPrice }
  else liftIO $ fail "Invalid price"

-- Servant API definition
type API = "price" :> ReqBody '[JSON] PricePoint :> Post '[JSON] Text
      :<|> "fibonacci" :> Get '[JSON] FibResponse
      :<|> "chart" :> Get '[JSON] Value

-- Server implementation
server :: ConnectionPool -> Server API
server pool = postPrice :<|> getFibonacci :<|> getChart
  where
    postPrice :: PricePoint -> Handler Text
    postPrice price = do
      processedPrice <- liftIO $ S.runStream $ processPriceStream price
      liftIO $ upsertPrice pool processedPrice
      return "Price added"

    getFibonacci :: Handler FibResponse
    getFibonacci = do
      prices <- liftIO $ queryPrices pool 100
      if null prices
        then throwError err400 { errBody = "No price data available" }
        else do
          let (high, low) = findSwingPoints (map snd prices)
              fibs = calcFibRetracement high low
              latestPrice = snd $ head prices
              inPocket = inGoldenPocket latestPrice fibs
              srLevels = calcSupportResistance (map snd prices)
          return $ FibResponse (zip fibLevels (A.toList fibs)) inPocket srLevels

    getChart :: Handler Value
    getChart = do
      prices <- liftIO $ queryPrices pool 100
      if null prices
        then throwError err400 { errBody = "No price data available" }
        else do
          let (high, low) = findSwingPoints (map snd prices)
              fibs = calcFibRetracement high low
              srLevels = calcSupportResistance (map snd prices)
          return $ toJSON $ plotChart prices fibs srLevels

-- Mock price feed with Streamly
mockPriceFeed :: ConnectionPool -> IO ()
mockPriceFeed pool = do
  S.drain $ S.repeatM (do
    time <- getCurrentTime
    price <- randomRIO (40000, 60000)
    threadDelay 1000000
    return $ PricePoint time price
    ) S.|> processPriceStream S.|> S.mapM_ (upsertPrice pool)

-- Main function
main :: IO ()
main = do
  let connStr = "host=localhost port=5432 user=postgres password=yourpassword dbname=trading"
  pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
  runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool
  forkIO $ mockPriceFeed pool
  run 8080 $ serve (Proxy :: Proxy API) (server pool)
