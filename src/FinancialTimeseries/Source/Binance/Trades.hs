{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Source.Binance.Trades where

import Control.Monad (mzero)

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Aeson as Ae
import qualified Data.Text as Text

import Text.Printf (PrintfArg, printf)

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)


data TradeQuery = TradeQuery {
  tradeUrl :: String
  , tradeSymbol :: Symbol
  , tradeLimit :: Maybe Int
  }

data Trade a = Trade {
  idty :: Integer
  , price :: a
  , qty :: Double
  , quoteQty :: Double
  , time :: UTCTime
  , isBuyerMaker :: Bool
  , isBestMatch :: Bool
  } deriving (Show)

instance (Read a) => Ae.FromJSON (Trade a) where
  parseJSON (Ae.Object o) = do
    i <- o Ae..: Text.pack "id"
    p <- fmap read (o Ae..: Text.pack "price")
    q <- fmap read (o Ae..: Text.pack "qty")
    qq <- fmap read (o Ae..: Text.pack "quoteQty")
    t <- fmap (posixSecondsToUTCTime . (/1000)) (o Ae..: Text.pack "time")
    buyer <- o Ae..: Text.pack "isBuyerMaker"
    best <- o Ae..: Text.pack "isBestMatch"
    return (Trade i p q qq t buyer best)
  parseJSON _ = mzero

instance (PrintfArg a) => Pretty (Trade a) where
  pretty (Trade i p q qq t buyer best) =
    printf "%8d%8f%8f%12f%30s%6s%6s\n"  i p q qq (show t) (show buyer) (show best)

instance (PrintfArg a) => Pretty [Trade a] where
  pretty ts =
    printf "%8s%8s%8s%12s%30s%6s%6s\n" "ID" "PRICE" "QTY" "QUOTEQTY" "TIME" "BUYER" "BEST"
    ++ concatMap pretty ts

defaultTradeQuery :: Symbol -> Maybe Int -> TradeQuery
defaultTradeQuery sym lim = TradeQuery {
  tradeUrl = binanceBaseUrl ++ "trades"
  , tradeSymbol = sym
  , tradeLimit = lim
  }

  
getTrades :: (Read a) => TradeQuery -> IO [Trade a]
getTrades tq = do
  let query =
        "symbol=" ++ pretty (tradeSymbol tq)
        ++ maybe "" (("&limit="++) . show) (tradeLimit tq)
      url = tradeUrl tq
      req = Simple.parseRequest_ (url ++ "?" ++ query)
      
  response <- Simple.httpJSON req

  return (Simple.getResponseBody response)


