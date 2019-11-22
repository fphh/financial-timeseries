

module FinancialTimeseries.Source.Binance.BookTicker where

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

import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Util (toNumber, toSymbol)

import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)



data Query = Query {
  endpoint :: String
  , symbol :: Maybe Symbol
  }

defaultQuery :: Maybe Symbol -> Query
defaultQuery sym = Query {
  endpoint = binanceBaseUrl ++ "ticker/bookTicker"
  , symbol = sym
  }

get :: (Read a) => Query -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol (Ask a, Bid a)))
get query = do
  let qstr = fmap (("?symbol="++) . show) (symbol query)
      url = endpoint query
      req = Simple.parseRequest_ (url ++ maybe "" id qstr)
      
  response <- Simple.httpJSON req
  now <- getCurrentTime

  let sym = Text.pack "symbol"
      askP = Text.pack "askPrice"
      askQ = Text.pack "askQty"
      bidP = Text.pack "bidPrice"
      bidQ = Text.pack "bidQty"

      f (Ae.Object obj) =
        let sy = 
              case symbol query of
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


