{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module FinancialTimeseries.Source.Binance.OrderBook where

import GHC.Generics (Generic)

import Control.Applicative (liftA2)
import Control.Monad (mzero)

import Data.Time (UTCTime, getCurrentTime)

import Data.Maybe (catMaybes)

import qualified Data.Vector as Vec

import qualified Data.List as List

import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM

import qualified Data.Text as Text

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Util (toNumber, toSymbol)

import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)



data OrderBookLimit =
  OBL5
  | OBL10
  | OBL20
  | OBL50
  | OBL100
  | OBL500
  | OBL1000
  | OBL5000
  deriving Show

instance Pretty OrderBookLimit where
  pretty x = show $
    case x of
      OBL5 -> 5 :: Integer
      OBL10 -> 10
      OBL20 -> 20
      OBL50 -> 50
      OBL100 -> 100
      OBL500 -> 500
      OBL1000 -> 1000
      OBL5000 -> 5000

data OrderBookQuery = OrderBookQuery {
  orderBookUrl :: String
  , orderBookSymbol :: Symbol
  , orderBookLimit :: OrderBookLimit
  }

defaultOrderBookQuery :: Symbol -> OrderBookLimit -> IO OrderBookQuery
defaultOrderBookQuery sym obl =
  return $ OrderBookQuery {
  orderBookUrl = binanceBaseUrl ++ "depth"
  , orderBookSymbol = sym
  , orderBookLimit = obl
  }

data Bid a = Bid {
  bidPrice :: a
  , bidQty :: Double
  } deriving (Show, Read, Functor)

instance (Read a) => Ae.FromJSON (Bid a) where
  parseJSON (Ae.Array as) = do
    let prc = toNumber (as Vec.! 0)
        qty = toNumber (as Vec.! 1)

    case (prc, qty) of
      (Just p, Just q) -> return (Bid p q)
      _ -> mzero

  parseJSON _ = mzero
  
data Ask a = Ask {
  askPrice :: a
  , askQty :: Double
  } deriving (Show, Read, Functor)

instance (Read a) => Ae.FromJSON (Ask a) where
  parseJSON (Ae.Array as) = do
    let prc = toNumber (as Vec.! 0)
        qty = toNumber (as Vec.! 1)

    case (prc, qty) of
      (Just p, Just q) -> return (Ask p q)
      _ -> mzero

  parseJSON _ = mzero
  
data OrderBookResponse a = OrderBookResponse {
  lastUpdateId :: Integer
  , bids :: [Bid a]
  , asks :: [Ask a]
  } deriving (Show, Read, Generic, Ae.FromJSON)

getOrderBook :: (Read a, Show a) => OrderBookQuery -> IO (OrderBookResponse a)
getOrderBook obq = do
  let query =
        "symbol=" ++ show (orderBookSymbol obq)
        ++ "&limit=" ++ pretty (orderBookLimit obq)
  
      url = orderBookUrl obq
      req = Simple.parseRequest_ (url ++ "?" ++ query)

  response <- Simple.httpJSON req
  print (Simple.getResponseBody response)
  return (Simple.getResponseBody response)

instance (Show a) => Pretty (OrderBookResponse a) where
  pretty (OrderBookResponse uid bs as) =
    "lastUpdateId:\t" ++ show uid ++ "\n"
    ++ "\tPRICE\t\tQUANTITY\n"
    ++ "bids (buyer price):\n"
    ++ List.intercalate "\n" (map (\(Bid a b) -> "\t" ++ show a ++ "\t\t" ++ show b) bs)
    ++ "\nasks (seller price):\n"
    ++ List.intercalate "\n" (map (\(Ask a b) -> "\t" ++ show a ++ "\t\t" ++ show b) as)


data BookTickerQuery = BookTickerQuery {
  bookTickerUrl :: String
  , bookTickerSymbol :: Maybe Symbol
  }

defaultBookTickerQuery :: Maybe Symbol -> BookTickerQuery
defaultBookTickerQuery sym = BookTickerQuery {
  bookTickerUrl = binanceBaseUrl ++ "ticker/bookTicker"
  , bookTickerSymbol = sym
  }

getBookTicker :: (Read a) => BookTickerQuery -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol (Ask a, Bid a)))
getBookTicker btq = do
  let query = fmap (("?symbol="++) . show) (bookTickerSymbol btq)
      url = bookTickerUrl btq
      req = Simple.parseRequest_ (url ++ maybe "" id query)
      
  response <- Simple.httpJSON req
  now <- getCurrentTime

  let sym = Text.pack "symbol"
      askP = Text.pack "askPrice"
      askQ = Text.pack "askQty"
      bidP = Text.pack "bidPrice"
      bidQ = Text.pack "bidQty"

      f (Ae.Object obj) =
        let sy = 
              case bookTickerSymbol btq of
                Nothing -> HM.lookup sym obj >>= toSymbol
                s@(Just _) -> s
            ap = HM.lookup askP obj >>= toNumber
            aq = HM.lookup askQ obj >>= toNumber
            bp = HM.lookup bidP obj >>= toNumber
            bq = HM.lookup bidQ obj >>= toNumber
        in liftA2 (,) sy (liftA2 (,) (liftA2 Ask ap aq) (liftA2 Bid bp bq))
      f _ = Nothing

      as =
        case Simple.getResponseBody response of
          obj@(Ae.Object _) -> HM.fromList (catMaybes [f obj])
          Ae.Array arr -> HM.fromList (catMaybes (Vec.toList (Vec.map f arr)))
          _ -> HM.empty
          

  return (ExchangeRate (now, as))


