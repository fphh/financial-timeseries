{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FinancialTimeseries.Source.Binance.Account where

import GHC.Generics (Generic)

import Control.Monad (mzero)

import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.Aeson as Ae
import qualified Data.Text as Text

import qualified Network.HTTP.Simple as Simple

import qualified FinancialTimeseries.Source.Binance.Type.Key as Key
import FinancialTimeseries.Source.Binance.BinanceBaseUrl (binanceBaseUrl)
import FinancialTimeseries.Source.Binance.MakeRequest (makeRequest)



data AccountQuery = AccountQuery {
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
    
  parseJSON _ = mzero


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


defaultAccountQuery :: IO AccountQuery
defaultAccountQuery = do
  now <- getPOSIXTime
  return $ AccountQuery {
    accountUrl = binanceBaseUrl ++ "account"
    , accountTimestamp = floor (now*1000)
    }

getAccount :: Key.Api -> Key.Secret -> AccountQuery -> IO Account
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

