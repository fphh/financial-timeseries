{-# LANGUAGE RecordWildCards #-}



module FinancialTimeseries.Source.Binance.Binance where

import Control.Applicative (liftA2)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Text as Text

import Text.Read (readMaybe)

import qualified Data.Aeson as Ae

import qualified Data.HashMap.Strict as HM

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.BarLength (BarLength(..))
import FinancialTimeseries.Source.Row (Row(..), Volume(..))
import FinancialTimeseries.Source.Binance.Symbol (Symbol)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (Price(..))

import FinancialTimeseries.Util.Pretty (pretty)



newtype Interval = Interval {
  unInterval :: Int
  } deriving (Show)


data RequestParams = RequestParams {
  baseUrl :: String
  , symbol :: Symbol
  , apiKey :: String
  , barLength :: BarLength
  , limit :: Maybe Int
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }

binanceBaseUrl :: String
binanceBaseUrl = "https://api.binance.com/api/v3/"

defaultRequestParams :: String -> Symbol -> BarLength -> RequestParams
defaultRequestParams apiKey sym len =
  RequestParams {
  baseUrl = binanceBaseUrl ++ "klines"
  , symbol = sym
  , apiKey = apiKey
  , barLength = len
  , limit = Nothing
  , from = Nothing
  , to = Nothing
  }

  
utcToMillis :: UTCTime -> Integer
utcToMillis = (1000*) . round . utcTimeToPOSIXSeconds

url :: RequestParams -> Simple.Request
url RequestParams{..} =
  let u = baseUrl
        ++ "?symbol=" ++ pretty symbol
        ++ "&interval=" ++ pretty barLength
        ++ maybe "" (\l -> "&limit=" ++ show l) limit
        ++ maybe "" (\d -> "&startTime=" ++ show (utcToMillis d)) from
        ++ maybe "" (\d -> "&endTime=" ++ show (utcToMillis d)) to
  in  Simple.parseRequest_ u



{-
[
  [
    1499040000000,      // Open time 0
    "0.01634790",       // Open 1
    "0.80000000",       // High 2
    "0.01575800",       // Low 3
    "0.01577100",       // Close 4
    "148976.11427815",  // Volume 5
    1499644799999,      // Close time
    "2434.19055334",    // Quote asset volume
    308,                // Number of trades
    "1756.87402397",    // Taker buy base asset volume
    "28.46694368",      // Taker buy quote asset volume
    "17928899.62484339" // Ignore.
  ]
]
-}

toNumber :: (Read a) => Ae.Value -> Maybe a
toNumber (Ae.String str) = readMaybe (Text.unpack str)
toNumber _ = Nothing
      
      
getDataHelper ::
  (Read a) =>
  RequestParams -> IO (Maybe (Vector (UTCTime, Row a)))
getDataHelper request = do
  response <- Simple.httpJSON (url request)
  let f (Ae.Array as) = do
        o <- fmap Price (toNumber (as Vec.! 1))
        h <- fmap Price (toNumber (as Vec.! 2))
        l <- fmap Price (toNumber (as Vec.! 3))
        c <- fmap Price (toNumber (as Vec.! 4))
        v <- fmap Volume (toNumber (as Vec.! 5))
        t <- case as Vec.! 6 of
               Ae.Number sec -> Just (posixSecondsToUTCTime (fromIntegral (round sec :: Integer) / 1000))
               _ -> Nothing
        
        let row = Row o h l c v

        return (t, row)
      f _ = Nothing
  
      us =
        case Simple.getResponseBody response of
          Ae.Array xs -> Vec.map f xs
          _ -> Vec.singleton Nothing

  return (sequence us)

  
getData ::
  (Read a) =>
  RequestParams -> IO (Maybe (Vector (UTCTime, Row a)))
getData req =
  case limit req of
    Nothing -> getDataHelper req
    Just x | x <= 1000 -> getDataHelper req
    
    Just x -> do
      v <- getDataHelper req
      let newReq = req {
            limit = Just (x - 1000)
            , to = fmap (fst . Vec.head) v
            }
      vs <- getData newReq
      return (liftA2 (Vec.++) vs v)


getSymbol ::
  (Read a) =>
  RequestParams -> IO (Maybe (TimeseriesRaw a))
getSymbol req = do
  ds <- getData req
  return (fmap (TimeseriesRaw (pretty (symbol req)) . Price . Vec.map (fmap (unPrice . close))) ds)

data PriceRequest = PriceRequest {
  priceBaseUrl :: String
  , priceSymbol :: Maybe Symbol
  }

defaultPriceRequest :: PriceRequest
defaultPriceRequest = PriceRequest {
  priceBaseUrl = binanceBaseUrl
  , priceSymbol = Nothing
  }

  
getTickerPriceHelper ::
  (Read a) =>
  PriceRequest -> IO (Price (UTCTime, HM.HashMap Symbol a))
getTickerPriceHelper (PriceRequest u s) = do
  let req = Simple.parseRequest_ (u ++ maybe "" (("?symbol="++) . pretty) s)
  
  response <- Simple.httpJSON req
  now <- getCurrentTime

  let price = Text.pack "price"
      sym = Text.pack "symbol"

      toSymbol (Ae.String str) = read (Text.unpack str)
      toSymbol _ = error "getTickerPriceHelper: cannot read symbol"

      f (Ae.Object obj) = (maybe (toSymbol (obj HM.! sym)) id s, (toNumber (obj HM.! price)))
      f _ = error "getTickerPriceHelper: cannot read object"
      
      as =
        case Simple.getResponseBody response of
          obj@(Ae.Object _) -> HM.fromList [f obj]
          Ae.Array arr -> HM.fromList (Vec.toList (Vec.map f arr))
          _ -> HM.empty
          
  return (Price (now, HM.mapMaybe id as))
  

getTickerPrice ::
  (Read a) =>
  PriceRequest -> IO (Price (UTCTime, HM.HashMap Symbol a))
getTickerPrice req =
  getTickerPriceHelper (req { priceBaseUrl = priceBaseUrl req ++ "ticker/price" })

getAvgPrice ::
  (Read a) =>
  PriceRequest -> IO (Price (UTCTime, HM.HashMap Symbol a))
getAvgPrice req =
  getTickerPriceHelper (req { priceBaseUrl = priceBaseUrl req ++ "avgPrice" })



{-
data AvgPriceRequest = AvgPriceRequest {
  avgBaseUrl :: String
  , avgSymbol :: Symbol
  }

defaultAvgPriceRequest :: Symbol -> AvgPriceRequest
defaultAvgPriceRequest sym = AvgPriceRequest {
  avgBaseUrl = binanceBaseUrl ++ "avgPrice"
  , avgSymbol = sym
  }

getAvgPrice ::
  (Read a) =>
  AvgPriceRequest -> IO (Maybe (Price (UTCTime, a)))
getAvgPrice (AvgPriceRequest u s) = do
  let req = Simple.parseRequest_ (u ++ "?symbol=" ++ pretty s)

  response <- Simple.httpJSON req

  let as =
        case Simple.getResponseBody response of
          Ae.Object obj -> HM.lookup (Text.pack "price") obj >>= toNumber
          _ -> Nothing
          
  now <- getCurrentTime
  return (fmap (\x -> Price (now, x)) as)


data TickerPriceRequest = TickerPriceRequest {
  tickerBaseUrl :: String
  , tickerSymbol :: Maybe Symbol
  }

defaultTickerRequest :: TickerPriceRequest
defaultTickerRequest = TickerPriceRequest {
  tickerBaseUrl = binanceBaseUrl ++ "ticker/price"
  , tickerSymbol = Nothing
  }
-}
