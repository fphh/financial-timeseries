{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Source.Binance.Binance where


import Control.Applicative (liftA2)


import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.Maybe (catMaybes)

import qualified Data.Vector as Vec
import Data.Vector (Vector)


import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM

import qualified Data.Text as Text


import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Row (Row(..), Volume(..))
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength(..))


import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Util (utcToMillis, toNumber, toSymbol)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (pretty)



data RequestParams = RequestParams {
  baseUrl :: String
  , symbol :: Symbol
  , apiKey :: String
  , barLength :: BarLength
  , limit :: Maybe Int
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }

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


url :: RequestParams -> Simple.Request
url RequestParams{..} =
  let u = baseUrl
        ++ "?symbol=" ++ pretty symbol
        ++ "&interval=" ++ pretty barLength
        ++ maybe "" (\l -> "&limit=" ++ show l) limit
        ++ maybe "" (\d -> "&startTime=" ++ show (utcToMillis d)) from
        ++ maybe "" (\d -> "&endTime=" ++ show (utcToMillis d)) to
  in  Simple.parseRequest_ u


      
getDataHelper ::
  (Read a) =>
  RequestParams -> IO (Maybe (Vector (UTCTime, Row ExchangeRate a)))
getDataHelper request = do
  response <- Simple.httpJSON (url request)
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
  RequestParams -> IO (Maybe (Vector (UTCTime, Row ExchangeRate a)))
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
  RequestParams -> IO (Maybe (TimeseriesRaw ExchangeRate a))
getSymbol req = do
  ds <- getData req
  return (fmap (TimeseriesRaw (pretty (symbol req)) . ExchangeRate . Vec.map (fmap (unExchangeRate . close))) ds)


-- -------------------------------------------------------------------------------

{-
getAvgExchangeRate ::
  (Read a) =>
  ExchangeRateRequest -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
getAvgExchangeRate req =
  getTickerExchangeRateHelper (req { priceBaseUrl = priceBaseUrl req ++ "avgExchangeRate" })
-}



-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------


-- ------------------------------------------------------------------------------


-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------

{-

truncAskOrderList :: (Ord a, Num a) => Double -> [Ask a] -> (a, a)
truncAskOrderList qty as =
  let suM = List.foldr (\(Ask p q) acc -> acc+q) 0
  
      go _ [] = []
      go n (Ask p q : xs) =
        case n <= 0 of
          True -> []
          False -> Ask p (min q n) : go (n-q) xs

      qs = go qty as

      f (Ask p q) = q * p

  in (suM qs, sum (map f qs))
    

askMarketPrice :: (Ord a, Fractional a) => Double -> [Ask a] -> a
askMarketPrice qty = (\(x, y) -> y/x) . truncAskOrderList qty

-}
