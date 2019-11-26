{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module FinancialTimeseries.Source.Binance.OrderBook where

import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.Aeson as Ae

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)

import FinancialTimeseries.Type.ByQuantity (ByQuantity(..))

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
    ++ List.intercalate "\n" (map (\(ByQuantity (Bid a) b) -> "\t" ++ show a ++ "\t\t" ++ show b) bs)
    ++ "\nasks (seller price):\n"
    ++ List.intercalate "\n" (map (\(ByQuantity (Ask a) b) -> "\t" ++ show a ++ "\t\t" ++ show b) as)



askByQuantity ::
  (Ord a, Num a, Fractional a) =>
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
  (Ord a, Num a, Fractional a) =>
  Double -> [ByQuantity (Bid a)] -> ByQuantity (Bid a)
bidByQuantity qty as =
  let go _ [] = []
      go n (ByQuantity (Bid p) q : xs) =
        case n <= 0 of
          True -> []
          False -> p * realToFrac (min q n) : go (n-q) xs

      qs = go qty as

  in ByQuantity (Bid (sum qs / realToFrac qty)) qty



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
