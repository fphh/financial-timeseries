
module FinancialTimeseries.Trade.LoggerData where

import qualified System.IO as Sys

import Data.Time (UTCTime)

import qualified Data.List as List

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Type.Types (Price(..))


import Debug.Trace (trace)


data LoggerData a = LoggerData {
  ltime :: UTCTime
  , lbaseCurrencyUSDT :: Maybe (Price a)
  , lcurrencyPair :: Price a
  , laccount :: Account a
  } deriving (Show, Read)

notAvailable :: String
notAvailable = "n/a"

logger ::
  (Show a) =>
  Sys.Handle -> LoggerData a -> IO ()
logger hd (LoggerData t bcusdt (Price cp) (Account bc qc)) =
  let bcl = maybe [notAvailable] ((:[]) . show . unPrice) bcusdt
      str = List.intercalate "," (show t : bcl ++ [show cp, show bc, show qc])
  in Sys.hPutStrLn hd str

-- Fault tollerant parsing ???
parse ::
  (Read a) =>
  FilePath -> IO [LoggerData a]
parse file = do
  txt <- readFile file
  let ls = lines txt
  
      f [t, bcusdt, cp, bc, qc] =
        let acnt = Account (read bc) (read qc)
            usdt =
              case bcusdt of
                x | x == notAvailable -> Nothing
                x -> Just (Price (read x))
        in LoggerData (read t) usdt (Price (read cp)) acnt
      f _ = error "LoggerData.parse: never here"
  
      brk xs =
        case break (==',') xs of
          ("", _) -> []
          (as, bs) ->
            as : case bs of
                   [] -> []
                   _:cs   -> brk cs
                             
  return (map (f . brk) ls)
  
