
module FinancialTimeseries.Source.Binance.TickerPrice where


import Control.Applicative (liftA2)

import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (catMaybes)
import qualified Data.Vector as Vec
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Util (toNumber, toSymbol)
import FinancialTimeseries.Type.Types (ExchangeRate(..))
import FinancialTimeseries.Util.Pretty (pretty)



data Query = Query {
  endpoint :: String
  , symbol :: Maybe Symbol
  }

defaultQuery :: Query
defaultQuery = Query {
  endpoint = binanceBaseUrl ++ "ticker/price"
  , symbol = Nothing
  }

  
get ::
  (Read a) =>
  Query -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
get (Query ept s) = do
  let req = Simple.parseRequest_ (ept ++ maybe "" (("?symbol="++) . pretty) s)
  
  response <- Simple.httpJSON req
  now <- getCurrentTime

  let price = Text.pack "price"
      sym = Text.pack "symbol"

      f (Ae.Object obj) =
        let sy = 
              case s of
                Nothing -> HM.lookup sym obj >>= toSymbol
                Just _ -> s
            num = HM.lookup price obj >>= toNumber
        in liftA2 (,) sy num 
      f _ = Nothing
      
      as =
        case Simple.getResponseBody response of
          obj@(Ae.Object _) -> HM.fromList (catMaybes [f obj])
          Ae.Array arr -> HM.fromList (catMaybes (Vec.toList (Vec.map f arr)))
          _ -> HM.empty
          
  return (ExchangeRate (now, HM.mapMaybe id as))
  

{-
getTickerExchangeRate ::
  (Read a) =>
  ExchangeRateQuery -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
getTickerExchangeRate req =
  getTickerExchangeRateHelper (req { priceBaseUrl = priceBaseUrl req ++ "ticker/price" })
-}
