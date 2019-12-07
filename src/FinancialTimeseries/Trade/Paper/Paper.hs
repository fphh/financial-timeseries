{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FinancialTimeseries.Trade.Paper.Paper where

import qualified System.Directory as Dir
import qualified System.IO as Sys

import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Control.Applicative (liftA2)

import Control.Monad (void, join, foldM_)

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime, formatTime, defaultTimeLocale, iso8601DateFormat)

import qualified Data.Vector as Vec

import qualified Data.HashMap.Strict as HM

import qualified Data.List as List

import qualified FinancialTimeseries.Algorithm.MovingAverage as MA

-- import qualified FinancialTimeseries.Source.Binance.Binance as Binance

-- FinancialTimeseries.Source.Binance.TickerPrice qualified

import qualified FinancialTimeseries.Source.Binance.Klines as Klines
import qualified FinancialTimeseries.Source.Binance.OrderBook as OrderBook
import qualified FinancialTimeseries.Source.Binance.TickerPrice as TickerPrice

import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength, nextTimeSlices)
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))

import qualified FinancialTimeseries.Source.Binance.Type.BarLength as BarLength
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Trade.LoggerData (LoggerData(..), logger)
import FinancialTimeseries.Type.ByQuantity (ByQuantity(..))
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Signal (Signal(..), lastSignal)
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, ExchangeRate(..))
import qualified FinancialTimeseries.Type.Timeseries as TS


import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)

import Debug.Trace (trace)


data Config params a = Config {
  outputDirPrefix :: String
  , account :: Account a
  , symbol :: Symbol
  , limit :: Int
  , fraction :: Fraction a
  , orderBookDepth :: OrderBook.Limit
  , parameters :: params
  , strategy :: params -> Strategy ExchangeRate a
  }


outputDir :: String
outputDir = "output"

refreshAccount ::
  (Num a, Fractional a, TS.Length (TS.TimeseriesRaw price a), Functor price, StripPrice price) =>
  Fraction a
  -> Strategy price a
  -> ExchangeRate (Bid a, Ask a)
  -> Account a
  -> TS.TimeseriesRaw price a
  -> Account a
refreshAccount (Fraction f) (Strategy stgy) bidAsk acnt@(Account bc qc) ts =
  let ms = stgy ts
      signal = lastSignal ms

      buy  (ExchangeRate (Bid prc, _)) = Account (bc - f*bc) (qc + f*bc*prc)
      sell (ExchangeRate (_, Ask prc)) = Account (bc + qc/prc) 0

  in case signal of
       -- Invest -> buy bidAsk
       -- DisInvest -> sell bidAsk

       
       Invest -> sell bidAsk
       DisInvest -> buy bidAsk
              
       None -> acnt


--       Buy -> Account (bc - f*bc) (qc + f*bc*prc)
--       Sell -> Account (bc + qc/prc) 0

type Message a = Either () (ExchangeRate (UTCTime, (Bid a, Ask a)))

{-
trader ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  MVar (ExchangeRate (UTCTime, (Bid a, Ask a))) -> BarLength -> Config params a -> IO ()
-}

trader ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  MVar (Message a) -> BarLength -> Config params a -> IO FilePath

trader mvar bl cfg = do

  now <- getCurrentTime

  let dirs =
        outputDir
        ++ "/" ++ outputDirPrefix cfg
        ++ "-"
        ++ formatTime defaultTimeLocale (iso8601DateFormat (Just "%X%Z")) now

  Dir.createDirectoryIfMissing True dirs
  
  let sym = symbol cfg

      fileNameCsv =
        dirs
        ++ "/" ++ show sym
        ++ "-" ++ toFileString bl
        ++ "-" ++ toFileString (parameters cfg)
        ++ ".csv"
        
      fileNameParams =
        dirs
        ++ "/" ++ show sym
        ++ "-" ++ toFileString bl
        ++ "-" ++ toFileString (parameters cfg)
        ++ ".params"

  writeFile fileNameParams (show (bl, parameters cfg))
  
  hd <- Sys.openFile fileNameCsv Sys.WriteMode
  Sys.hSetBuffering hd Sys.LineBuffering

  putStrLn ("Starting trader for " ++ fileNameCsv)



  let tsReq = (Klines.defaultQuery sym bl) {
        Klines.limit = Just (limit cfg + 100)
        }
      
      refreshAcc = refreshAccount (fraction cfg) ((strategy cfg) (parameters cfg))

      newData acnt bidAsk = do
        
        Just as <- Klines.get tsReq

        let prc = fmap (fmap (\(Bid b, Ask a) -> (a+b)/2)) bidAsk
        
            newAcnt = refreshAcc (fmap snd bidAsk) acnt as
            
            bc@(ExchangeRate (t, _)) = TS.last as
            
            loggerData = LoggerData {
              ltime = t
              , bidAndAsk = fmap snd bidAsk
              , lcurrencyPair = fmap snd bc
              , laccount = newAcnt
              }
              
        logger hd loggerData

        loop newAcnt

      end = const (return fileNameCsv)
  
      loop acnt = takeMVar mvar >>= either end (newData acnt)

        
  loop (account cfg)



ticker ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  (BarLength, [(Config params a,  MVar (Message a))]) -> IO ()
ticker (bl, mcfgs) = do
  ts <- nextTimeSlices bl

  let finally mvar filePath = do
        print filePath
        putMVar mvar (Left ())
        
      g (c, mvar) = void (forkFinally (trader mvar bl c) (finally mvar))

  mapM g mcfgs

  let f (cfg, mvar) = do
        let sym = symbol cfg
            depth = orderBookDepth cfg

        ob <- OrderBook.get (OrderBook.defaultQuery sym depth)

        let bidAsk = fmap (fmap byQuantity) (OrderBook.exchangeRateByQuantity 10 ob)
        
        putMVar mvar (Right bidAsk)

      fs = map (forkIO . f) mcfgs
 
      h t = do
        now <- getCurrentTime
        let delta = t `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))
        sequence_ fs
        xnow <- getCurrentTime
        putStrLn ("Received ticker for " ++ show (map (symbol . fst) mcfgs) ++ " at " ++ show xnow)
        
  mapM_ h ts

  
wait16hours :: IO ()
wait16hours = threadDelay (1000*1000*60*60*16)

wait1minute :: IO ()
wait1minute = threadDelay (1000*1000*60*3)

addMVars :: [(BarLength, [Config params a])] -> IO [(BarLength, [(Config params a, MVar (Message a))])]
addMVars xs = do
  let g x = sequence (x, newEmptyMVar)
  mapM (sequence . fmap (mapM g)) xs


sendEnd :: [(BarLength, [(Config params a, MVar (Message a))])] -> IO ()
sendEnd xs = do
  let g (_, mvar) = do
        putMVar mvar (Left ())
        void (takeMVar mvar)
        
  mapM_ (sequence . fmap (mapM g)) xs  
  
{-
start ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  [(BarLength, [Config params a])] -> IO ()
start xs = do
  let io = mapM_ (forkIO . ticker) xs
  io
  threadDelay (1000*1000*60*60*16) -- 16 hours
-}

start ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  [(BarLength, [Config params a])] -> IO ()
start xs = do
  cs <- addMVars xs
  mapM_ (forkIO . ticker) cs
  -- wait16hours
  wait1minute
  sendEnd cs
  
  -- threadDelay (1000*1000*60*60*16) -- 16 hours
  -- threadDelay (1000*1000*60) -- 16 hours
