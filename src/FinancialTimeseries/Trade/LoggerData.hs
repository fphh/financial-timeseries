
module FinancialTimeseries.Trade.LoggerData where

import qualified System.IO as Sys

import Data.Time (UTCTime)

import qualified Data.List as List

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Type.Types (Price(..))


data LoggerData a = LoggerData {
  ltime :: UTCTime
  , lbaseCurrencyUSDT :: Maybe (Price a)
  , lcurrencyPair :: Price a
  , laccount :: Account a
  } deriving (Show, Read)

logger ::
  (Show a) =>
  Sys.Handle -> LoggerData a -> IO ()
logger hd (LoggerData t bcusdt (Price cp) (Account bc qc)) =
  let bcl = maybe [] ((:[]) . show . unPrice) bcusdt

      str = List.intercalate "," (show t : bcl ++ [show cp, show bc, show qc])
  in Sys.hPutStrLn hd str
