{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module FinancialTimeseries.Source.Binance.Binance where

import GHC.Generics (Generic)

import Control.Applicative (liftA2)
import Control.Monad (mzero)

import Data.Char (ord)

import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import qualified Data.Text as Text

import qualified Data.CaseInsensitive as CI

import Text.Read (readMaybe)

import Text.Printf (printf)

import Crypto.Hash.SHA256 (hmac)

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Binance.BarLength (BarLength(..))
import FinancialTimeseries.Source.Row (Row(..), Volume(..))
import FinancialTimeseries.Source.Binance.Symbol (Symbol)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (pretty)

import Debug.Trace (trace)

newtype Interval = Interval {
  unInterval :: Int
  } deriving (Show)


data RequestParams = RequestParams {
  baseUrl :: String
  , symbol :: Symbol
  , apiKey :: String
  , barLength :: BarLength
  , limit :: Maybe Int
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }

binanceBaseUrl :: String
binanceBaseUrl = "https://api.binance.com/api/v3/"

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

      toSymbol (Ae.String str) = read (Text.unpack str)
      toSymbol _ = error "getTickerExchangeRateHelper: cannot read symbol"

      f (Ae.Object obj) = (maybe (toSymbol (obj HM.! sym)) id s, (toNumber (obj HM.! price)))
      f _ = error "getTickerExchangeRateHelper: cannot read object"
      
      as =
        case Simple.getResponseBody response of
          obj@(Ae.Object _) -> HM.fromList [f obj]
          Ae.Array arr -> HM.fromList (Vec.toList (Vec.map f arr))
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

data BalanceRequest = BalanceRequest {
  accountUrl :: String
  , accountTimestamp :: Int
  , accountApiKey :: ApiKey
  , accountSecretKey :: SecretKey
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


data Balance = Balance {
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


defaultBalanceRequest :: ApiKey -> SecretKey -> IO BalanceRequest
defaultBalanceRequest apiKey secretKey = do
  now <- getPOSIXTime
  return $ BalanceRequest {
    accountUrl = binanceBaseUrl ++ "account"
    , accountTimestamp = floor (now*1000)
    , accountApiKey = apiKey
    , accountSecretKey = secretKey
    }

getBalance :: BalanceRequest -> IO Balance
getBalance accReq = do
  let f :: Char -> String
      f = printf "%02x" . ord
      secret = BS8.pack (unSecretKey (accountSecretKey accReq))
      query = "timestamp=" ++ show (accountTimestamp accReq)
      digest = concatMap f (BS8.unpack (hmac secret (BS8.pack query)))
      fullQuery = query ++ "&signature=" ++ digest

      url = accountUrl accReq
      req = Simple.parseRequest_ (url ++ "?" ++ fullQuery)

      reqWithHeader = Simple.addRequestHeader
        (CI.mk (BS8.pack "X-MBX-APIKEY")) (BS8.pack (unApiKey (accountApiKey accReq))) req

  response <- Simple.httpJSON reqWithHeader
  
  return (Simple.getResponseBody response)

