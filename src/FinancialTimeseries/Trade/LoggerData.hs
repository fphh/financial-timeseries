
module FinancialTimeseries.Trade.LoggerData where

import qualified System.IO as Sys

import Data.Time (UTCTime)

import qualified Data.List as List

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, price)



data LoggerData price a = LoggerData {
  ltime :: UTCTime
  , lbaseCurrencyUSDT :: Maybe (price a)
  , lcurrencyPair :: price a
  , laccount :: Account a
  } deriving (Show, Read)

notAvailable :: String
notAvailable = "n/a"

logger ::
  (Show a, StripPrice price) =>
  Sys.Handle -> LoggerData price a -> IO ()
logger hd (LoggerData t bcusdt cp (Account bc qc)) =
  let bcl = maybe [notAvailable] ((:[]) . show . stripPrice) bcusdt
      str = List.intercalate "," (show t : bcl ++ [show (stripPrice cp), show bc, show qc])
  in Sys.hPutStrLn hd str

-- Fault tollerant parsing ???
parse ::
  (Read a, StripPrice price) =>
  FilePath -> IO [LoggerData price a]
parse file = do
  txt <- readFile file
  let ls = lines txt
  
      f [t, bcusdt, cp, bc, qc] =
        let acnt = Account (read bc) (read qc)
            usdt =
              case bcusdt of
                x | x == notAvailable -> Nothing
                x -> Just (price (read x))
        in LoggerData (read t) usdt (price (read cp)) acnt
      f _ = error "LoggerData.parse: never here"
  
      brk xs =
        case break (==',') xs of
          ("", _) -> []
          (as, bs) ->
            as : case bs of
                   [] -> []
                   _:cs   -> brk cs
                             
  return (map (f . brk) ls)
  
