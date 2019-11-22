

module FinancialTimeseries.Source.Binance.MakeRequest where


import Data.Char (ord)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI

import Text.Printf (printf)

import Crypto.Hash.SHA256 (hmac)

import qualified Network.HTTP.Simple as Simple

import qualified FinancialTimeseries.Source.Binance.Type.Key as Key




type Query = String
type Url = String

makeRequest :: Key.Api -> Key.Secret -> Url -> Query -> Simple.Request
makeRequest (Key.Api apiKey) (Key.Secret secretKey) url query =
  let f = printf "%02x" . ord
      secret = BS8.pack secretKey
      digest = concatMap f (BS8.unpack (hmac secret (BS8.pack query)))
      fullQuery = query ++ "&signature=" ++ digest
      req = Simple.parseRequest_ (url ++ "?" ++ fullQuery)
      reqWithHeader = Simple.addRequestHeader (CI.mk (BS8.pack "X-MBX-APIKEY")) (BS8.pack apiKey) req
  in reqWithHeader
