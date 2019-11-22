
module FinancialTimeseries.Source.Binance.OrderTest where

import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.Aeson as Ae

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.MakeRequest (makeRequest)
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import qualified FinancialTimeseries.Source.Binance.Type.Key as Key
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)


data OrderType =
  -- LIMIT
  MARKET
  deriving (Show)

data OrderSide =
  BUY
  | SELL
  deriving (Show)

newtype Quantity = Quantity {
  unQuantity :: Double
  }

data OrderTestRequest = OrderTestRequest {
  orderTestUrl :: String
  , orderTestTimestamp :: Int
  , orderTestType :: OrderType
  , orderTestSide :: OrderSide
  , orderTestSymbol :: Symbol
  , orderTestQuantity :: Quantity
  }


defaultOrderTestRequest :: OrderType -> OrderSide -> Symbol -> Quantity -> IO OrderTestRequest
defaultOrderTestRequest ty side sym qty = do
  now <- getPOSIXTime
  return $ OrderTestRequest {
    orderTestUrl = "POST " ++ binanceBaseUrl ++ "order/test"
    , orderTestTimestamp = floor (now*1000)
    , orderTestType = ty
    , orderTestSide = side
    , orderTestSymbol = sym
    , orderTestQuantity = qty
    }


getOrderTest :: Key.Api -> Key.Secret -> OrderTestRequest -> IO Ae.Value
getOrderTest apiKey secretKey otReq = do
  let query =
        "timestamp=" ++ show (orderTestTimestamp otReq)
        ++ "&symbol=" ++ show (orderTestSymbol otReq)
        ++ "&side=" ++ show (orderTestSide otReq)
        ++ "&type=" ++ show (orderTestType otReq)
        ++ "&quantity=" ++ show (unQuantity (orderTestQuantity otReq))
  
      url = orderTestUrl otReq
      req = makeRequest apiKey secretKey url query

  response <- Simple.httpJSON req  
  return (Simple.getResponseBody response)

