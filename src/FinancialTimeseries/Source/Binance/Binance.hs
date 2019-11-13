{-# LANGUAGE RecordWildCards #-}



module FinancialTimeseries.Source.Binance.Binance where

import Control.Applicative (liftA2)

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Text as Text

import Text.Read (readMaybe)

import qualified Data.Aeson as Ae

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
binanceBaseUrl = "https://api.binance.com/api/v3/klines"

defaultRequestParams :: String -> Symbol -> BarLength -> RequestParams
defaultRequestParams apiKey sym len =
  RequestParams {
  baseUrl = binanceBaseUrl
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

getDataHelper :: RequestParams -> IO (Maybe (Vector (UTCTime, Row)))
getDataHelper request = do
  response <- Simple.httpJSON (url request)
  let toNumber (Ae.String str) = readMaybe (Text.unpack str)
      toNumber _ = Nothing
      
      f (Ae.Array as) = do
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

  
getData :: RequestParams -> IO (Maybe (Vector (UTCTime, Row)))
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


getSymbol :: RequestParams -> IO (Maybe (TimeseriesRaw Double))
getSymbol req = do
  ds <- getData req
  return (fmap (TimeseriesRaw (pretty (symbol req)) . Price . Vec.map (fmap (unPrice . close))) ds)

