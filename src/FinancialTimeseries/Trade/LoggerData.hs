
module FinancialTimeseries.Trade.LoggerData where

import qualified System.IO as Sys

import Data.Time (UTCTime)

import qualified Data.List as List

import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, price)




data LoggerData price a = LoggerData {
  ltime :: UTCTime
  , bidAndAsk :: price (Bid a, Ask a)
  , lcurrencyPair :: price a
  , laccount :: Account a
  } --  deriving (Show, Read)

notAvailable :: String
notAvailable = "n/a"

logger ::
  (Show a, StripPrice price) =>
  Sys.Handle -> LoggerData price a -> IO ()
logger hd (LoggerData t bidAsk cp (Account bc qc)) =
  let str = List.intercalate "," $
        show t
        : show (unBid (fst (stripPrice bidAsk)))
        : show (unAsk (snd (stripPrice bidAsk)))
        : show (stripPrice cp)
        : show bc
        : show qc
        : []
  in Sys.hPutStrLn hd str

-- Fault tollerant parsing ???
parse ::
  (Read a, StripPrice price) =>
  FilePath -> IO [LoggerData price a]
parse file = do
  txt <- readFile file
  let ls = lines txt
  
      f [t, b, a, cp, bc, qc] =
        let bidAsk = price (Bid (read b), Ask (read a))
        in LoggerData (read t) bidAsk (price (read cp)) (Account (read bc) (read qc))
      f _ = error "LoggerData.parse: never here"
  
      brk xs =
        case break (==',') xs of
          ("", _) -> []
          (as, bs) ->
            as : case bs of
                   [] -> []
                   _:cs   -> brk cs
                             
  return (map (f . brk) ls)
  
