{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module FinancialTimeseries.Source.Binance.OrderBook where

import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.Aeson as Ae

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)



data Limit =
  OBL5
  | OBL10
  | OBL20
  | OBL50
  | OBL100
  | OBL500
  | OBL1000
  | OBL5000
  deriving Show

instance Pretty Limit where
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
  , bids :: [Bid a]
  , asks :: [Ask a]
  } deriving (Show, Read, Generic, Ae.FromJSON)


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

get :: (Read a, Show a) => Query -> IO (Response a)
get query = do
  let qstr =
        "symbol=" ++ show (symbol query)
        ++ "&limit=" ++ pretty (limit query)
  
      url = endpoint query
      req = Simple.parseRequest_ (url ++ "?" ++ qstr)

  response <- Simple.httpJSON req
  return (Simple.getResponseBody response)


instance (Show a) => Pretty (Response a) where
  pretty (Response uid bs as) =
    "lastUpdateId:\t" ++ show uid ++ "\n"
    ++ "\tPRICE\t\tQUANTITY\n"
    ++ "bids (buyer price):\n"
    ++ List.intercalate "\n" (map (\(Bid a b) -> "\t" ++ show a ++ "\t\t" ++ show b) bs)
    ++ "\nasks (seller price):\n"
    ++ List.intercalate "\n" (map (\(Ask a b) -> "\t" ++ show a ++ "\t\t" ++ show b) as)

