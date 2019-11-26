{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module FinancialTimeseries.Source.Binance.OrderBook where

import GHC.Generics (Generic)

import Data.Time (UTCTime, getCurrentTime)

import qualified Data.List as List
import qualified Data.Aeson as Ae

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)

import FinancialTimeseries.Type.ByQuantity (ByQuantity(..))
import FinancialTimeseries.Type.Timed (Timed(..), middle)
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)



data Limit =
  Depth5
  | Depth10
  | Depth20
  | Depth50
  | Depth100
  | Depth500
  | Depth1000
  | Depth5000
  deriving (Show)

instance Pretty Limit where
  pretty x = show $
    case x of
      Depth5 -> 5 :: Integer
      Depth10 -> 10
      Depth20 -> 20
      Depth50 -> 50
      Depth100 -> 100
      Depth500 -> 500
      Depth1000 -> 1000
      Depth5000 -> 5000

data Query = Query {
  endpoint :: String
  , symbol :: Symbol
  , limit :: Limit
  }

defaultQuery :: Symbol -> Limit -> Query
defaultQuery sym lim = Query {
  endpoint = binanceBaseUrl ++ "depth"
  , symbol = sym
  , limit = lim
  }

  
data Response a = Response {
  lastUpdateId :: Integer
  , bids :: [ByQuantity (Bid a)]
  , asks :: [ByQuantity (Ask a)]
  } deriving (Show, Read, Eq, Generic, Ae.FromJSON)




get :: (Read a, Show a) => Query -> IO (Timed (Response a))
get query = do
  let qstr =
        "symbol=" ++ show (symbol query)
        ++ "&limit=" ++ pretty (limit query)
  
      url = endpoint query
      req = Simple.parseRequest_ (url ++ "?" ++ qstr)

  s <- getCurrentTime
  response <- Simple.httpJSON req
  e <- getCurrentTime
  
  return (Timed s e (Simple.getResponseBody response))


instance (Show a) => Pretty (Response a) where
  pretty (Response uid bs as) =
    "lastUpdateId:\t" ++ show uid ++ "\n"
    ++ "\tPRICE\t\tQUANTITY\n"
    ++ "bids (buyer price):\n"
    ++ List.intercalate "\n" (map (\(ByQuantity (Bid a) b) -> "\t" ++ show a ++ "\t\t" ++ show b) bs)
    ++ "\nasks (seller price):\n"
    ++ List.intercalate "\n" (map (\(ByQuantity (Ask a) b) -> "\t" ++ show a ++ "\t\t" ++ show b) as)



askByQuantity ::
  (Ord a, Fractional a) =>
  Double -> [ByQuantity (Ask a)] -> ByQuantity (Ask a)
askByQuantity qty as =
  let go _ [] = []
      go n (ByQuantity (Ask p) q : xs) =
        case n <= 0 of
          True -> []
          False -> p * realToFrac (min q n) : go (n-q) xs

      qs = go qty as

  in ByQuantity (Ask (sum qs / realToFrac qty)) qty

bidByQuantity ::
  (Ord a, Fractional a) =>
  Double -> [ByQuantity (Bid a)] -> ByQuantity (Bid a)
bidByQuantity qty as =
  let go _ [] = []
      go n (ByQuantity (Bid p) q : xs) =
        case n <= 0 of
          True -> []
          False -> p * realToFrac (min q n) : go (n-q) xs

      qs = go qty as

  in ByQuantity (Bid (sum qs / realToFrac qty)) qty

exchangeRateByQuantity ::
  (Ord a, Fractional a) =>
  Double -> Timed (Response a) -> (UTCTime, ExchangeRate (ByQuantity (Bid a, Ask a)))
exchangeRateByQuantity qty t =
  let ByQuantity us _ = bidByQuantity qty (bids (timed t))
      ByQuantity ws _ = askByQuantity qty (asks (timed t))
  in (middle t, ExchangeRate (ByQuantity (us, ws) qty))
  
{-
exchangeRateByQuantity ::
  Double -> Response a -> (UTCTime, (Bid a, Ask a))
exchangeRateByQuantity qty vs =
  let f (CollectedData start end (OrderBook.Response _ bs as)) =
        let t = ((end `diffUTCTime` start) / 2) `addUTCTime` start
            ByQuantity us _ = OrderBook.bidByQuantity qty bs
            ByQuantity ws _ = OrderBook.askByQuantity qty as
        in (t, (us, ws))
      g x qty = ExchangeRate (ByQuantity x qty)
  in TimeseriesRaw {
    name = nam
    , timeseries = ExchangeRateByQuantity (ExchangeRate (ByQuantity (Vec.map f vs) qty))
    }
-}

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
