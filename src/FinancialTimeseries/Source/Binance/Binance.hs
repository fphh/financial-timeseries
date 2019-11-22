{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Source.Binance.Binance where

import GHC.Generics (Generic)

import Control.Applicative (liftA2)
import Control.Monad (mzero)

import Data.Char (ord)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import Data.Maybe (catMaybes)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import qualified Data.Text as Text

import qualified Data.CaseInsensitive as CI

import Text.Read (readMaybe)

import Text.Printf (PrintfArg, printf)

import Crypto.Hash.SHA256 (hmac)

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Row (Row(..), Volume(..))
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol(Symbol))
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.DistributivePair (undistributePair)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)

import Debug.Trace (trace)

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

  
utcToMillis :: UTCTime -> Integer
utcToMillis = (1000*) . round . utcTimeToPOSIXSeconds

url :: RequestParams -> Simple.Request
url RequestParams{..} =
  let u = baseUrl
        ++ "?symbol=" ++ pretty symbol
        ++ "&interval=" ++ pretty barLength
        ++ maybe "" (\l -> "&limit=" ++ show l) limit
        ++ maybe "" (\d -> "&startTime=" ++ show (utcToMillis d)) from
        ++ maybe "" (\d -> "&endTime=" ++ show (utcToMillis d)) to
  in  Simple.parseRequest_ u


toNumber :: (Read a) => Ae.Value -> Maybe a
toNumber (Ae.String str) = readMaybe (Text.unpack str)
toNumber _ = Nothing

toSymbol :: Ae.Value -> Maybe Symbol
toSymbol (Ae.String str) =
  case readMaybe (Text.unpack str) of
    Nothing -> Just (Symbol (Text.unpack str))
    x -> x
toSymbol _ = Nothing

      
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

data ExchangeRateRequest = ExchangeRateRequest {
  priceBaseUrl :: String
  , priceSymbol :: Maybe Symbol
  }

defaultExchangeRateRequest :: ExchangeRateRequest
defaultExchangeRateRequest = ExchangeRateRequest {
  priceBaseUrl = binanceBaseUrl
  , priceSymbol = Nothing
  }

  
getTickerExchangeRateHelper ::
  (Read a) =>
  ExchangeRateRequest -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
getTickerExchangeRateHelper (ExchangeRateRequest u s) = do
  let req = Simple.parseRequest_ (u ++ maybe "" (("?symbol="++) . pretty) s)
  
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
  

getTickerExchangeRate ::
  (Read a) =>
  ExchangeRateRequest -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
getTickerExchangeRate req =
  getTickerExchangeRateHelper (req { priceBaseUrl = priceBaseUrl req ++ "ticker/price" })

getAvgExchangeRate ::
  (Read a) =>
  ExchangeRateRequest -> IO (ExchangeRate (UTCTime, HM.HashMap Symbol a))
getAvgExchangeRate req =
  getTickerExchangeRateHelper (req { priceBaseUrl = priceBaseUrl req ++ "avgExchangeRate" })

newtype ApiKey = ApiKey {
  unApiKey :: String
  }

newtype SecretKey = SecretKey {
  unSecretKey :: String
  }

data AccountRequest = AccountRequest {
  accountUrl :: String
  , accountTimestamp :: Int
  }


data Asset = Asset {
  asset :: String
  , free :: Double
  , locked :: Double
  } deriving (Show)

instance Ae.FromJSON Asset where
  parseJSON (Ae.Object v) = do
    let assetKey = Text.pack "asset"
        freeKey = Text.pack "free"
        lockedKey = Text.pack "locked"

        free = fmap read (v Ae..: freeKey)
        locked = fmap read (v Ae..: lockedKey)
        asset = v Ae..: assetKey
    
    Asset <$> asset <*> free <*> locked
    
  parseJSON _          = mzero


data Account = Account {
  takerCommission :: Double
  , buyerCommission :: Double
  , sellerCommission :: Double
  , makerCommission :: Double
  , accountType :: String
  , canDeposit :: Bool
  , canWithdraw :: Bool
  , canTrade :: Bool
  , balances :: [Asset]
  } deriving (Show, Generic, Ae.FromJSON)


defaultAccountRequest :: IO AccountRequest
defaultAccountRequest = do
  now <- getPOSIXTime
  return $ AccountRequest {
    accountUrl = binanceBaseUrl ++ "account"
    , accountTimestamp = floor (now*1000)
    }

type Query = String
type Url = String

makeRequest :: ApiKey -> SecretKey -> Url -> Query -> Simple.Request
makeRequest (ApiKey apiKey) (SecretKey secretKey) url query =
  let f = printf "%02x" . ord
      secret = BS8.pack secretKey
      digest = concatMap f (BS8.unpack (hmac secret (BS8.pack query)))
      fullQuery = query ++ "&signature=" ++ digest
      req = Simple.parseRequest_ (url ++ "?" ++ fullQuery)
      reqWithHeader = Simple.addRequestHeader (CI.mk (BS8.pack "X-MBX-APIKEY")) (BS8.pack apiKey) req
  in reqWithHeader

getAccount :: ApiKey -> SecretKey -> AccountRequest -> IO Account
getAccount apiKey secretKey accReq = do
  let query = "timestamp=" ++ show (accountTimestamp accReq)
      url = accountUrl accReq
      req = makeRequest apiKey secretKey url query
  
  response <- Simple.httpJSON req

  let p (Asset _ a b) = a /= 0 || b /= 0
      filterBalances acnt = acnt {
        balances = filter p (balances acnt)
        }
  
  return (filterBalances (Simple.getResponseBody response))


-- ------------------------------------------------------------------------------


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


getOrderTest :: ApiKey -> SecretKey -> OrderTestRequest -> IO Ae.Value
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


-- ------------------------------------------------------------------------------


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
      OBL5 -> 5
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
      askPrice = Text.pack "askPrice"
      askQty = Text.pack "askQty"
      bidPrice = Text.pack "bidPrice"
      bidQty = Text.pack "bidQty"

      f (Ae.Object obj) =
        let sy = 
              case bookTickerSymbol btq of
                Nothing -> HM.lookup sym obj >>= toSymbol
                s@(Just _) -> s
            ap = HM.lookup askPrice obj >>= toNumber
            aq = HM.lookup askQty obj >>= toNumber
            bp = HM.lookup bidPrice obj >>= toNumber
            bq = HM.lookup bidQty obj >>= toNumber
        in liftA2 (,) sy (liftA2 (,) (liftA2 Ask ap aq) (liftA2 Bid bp bq))
      f _ = Nothing

      as =
        case Simple.getResponseBody response of
          obj@(Ae.Object _) -> HM.fromList (catMaybes [f obj])
          Ae.Array arr -> HM.fromList (catMaybes (Vec.toList (Vec.map f arr)))
          _ -> HM.empty
          

  return (ExchangeRate (now, as))



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
