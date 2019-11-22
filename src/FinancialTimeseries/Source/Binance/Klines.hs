

module FinancialTimeseries.Source.Binance.Klines where

import Control.Applicative (liftA2)


import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


import qualified Data.Vector as Vec
import Data.Vector (Vector)


import qualified Data.Aeson as Ae



import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Row (Row(..), Volume(..))
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength(..))

import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Util (utcToMillis, toNumber)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (pretty)


data Query = Query {
  endpoint :: String
  , symbol :: Symbol
  , barLength :: BarLength
  , limit :: Maybe Int
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }

defaultQuery :: Symbol -> BarLength -> Query
defaultQuery sym len = Query {
  endpoint = binanceBaseUrl ++ "klines"
  , symbol = sym
  , barLength = len
  , limit = Nothing
  , from = Nothing
  , to = Nothing
  }


getDataHelper ::
  (Read a) =>
  Query -> IO (Maybe (Vector (UTCTime, Row ExchangeRate a)))
getDataHelper query = do
  let url =
        endpoint query
        ++ "?symbol=" ++ pretty (symbol query)
        ++ "&interval=" ++ pretty (barLength query)
        ++ maybe "" (\l -> "&limit=" ++ show l) (limit query)
        ++ maybe "" (\d -> "&startTime=" ++ show (utcToMillis d)) (from query)
        ++ maybe "" (\d -> "&endTime=" ++ show (utcToMillis d)) (to query)

      req = Simple.parseRequest_ url
      
  response <- Simple.httpJSON req
  
  let f (Ae.Array as) = do
        o <- fmap ExchangeRate (toNumber (as Vec.! 1))
        h <- fmap ExchangeRate (toNumber (as Vec.! 2))
        l <- fmap ExchangeRate (toNumber (as Vec.! 3))
        c <- fmap ExchangeRate (toNumber (as Vec.! 4))
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
  Query -> IO (Maybe (Vector (UTCTime, Row ExchangeRate a)))
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



get ::
  (Read a) =>
  Query -> IO (Maybe (TimeseriesRaw ExchangeRate a))
get req = do
  ds <- getData req
  return (fmap (TimeseriesRaw (pretty (symbol req)) . ExchangeRate . Vec.map (fmap (unExchangeRate . close))) ds)

