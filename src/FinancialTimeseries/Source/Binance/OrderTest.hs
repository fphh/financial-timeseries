
module FinancialTimeseries.Source.Binance.OrderTest where

import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.Aeson as Ae

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.MakeRequest (makeRequest)
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import qualified FinancialTimeseries.Source.Binance.Type.Key as Key
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)

import FinancialTimeseries.Type.Types (Quantity(..))


data OrderType =
  -- LIMIT
  MARKET
  deriving (Show)

data OrderSide =
  BUY
  | SELL
  deriving (Show)

data Query = Query {
  endpoint :: String
  , timestamp :: Int
  , type_ :: OrderType
  , side :: OrderSide
  , symbol :: Symbol
  , quantity :: Quantity Double
  }


defaultQuery :: OrderType -> OrderSide -> Symbol -> Quantity Double -> IO Query
defaultQuery ty side sym qty = do
  now <- getPOSIXTime
  return $ Query {
    endpoint = "POST " ++ binanceBaseUrl ++ "order/test"
    , timestamp = floor (now*1000)
    , type_ = ty
    , side = side
    , symbol = sym
    , quantity = qty
    }


get :: Key.Api -> Key.Secret -> Query -> IO Ae.Value
get apiKey secretKey query = do
  let qstr =
        "timestamp=" ++ show (timestamp query)
        ++ "&type=" ++ show (type_ query)
        ++ "&side=" ++ show (side query)
        ++ "&symbol=" ++ show (symbol query)
        ++ "&quantity=" ++ show (unQuantity (quantity query))
  
      url = endpoint query
      req = makeRequest apiKey secretKey url qstr

  response <- Simple.httpJSON req  
  return (Simple.getResponseBody response)

