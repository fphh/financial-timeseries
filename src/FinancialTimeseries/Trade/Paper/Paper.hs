{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FinancialTimeseries.Trade.Paper.Paper where

import qualified System.IO as Sys

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Control.Applicative (liftA2)

import Control.Monad (void, join, foldM_)

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime, formatTime, defaultTimeLocale, iso8601DateFormat)

import qualified Data.Vector as Vec

import qualified Data.HashMap.Strict as HM

import qualified Data.List as List

import qualified FinancialTimeseries.Algorithm.MovingAverage as MA

import qualified FinancialTimeseries.Source.Binance.BarLength as BarLength
import FinancialTimeseries.Source.Binance.BarLength (BarLength, nextTimeSlices)
import qualified FinancialTimeseries.Source.Binance.Binance as Binance
import FinancialTimeseries.Source.Binance.Symbol (Symbol)

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Trade.LoggerData (LoggerData(..), logger)
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Signal (Signal(..), lastSignal)
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, ExchangeRate(..))
import qualified FinancialTimeseries.Type.Timeseries as TS


import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)


data Config params a = Config {
  account :: Account a
  , symbol :: Symbol
  , usdtSymbol :: Maybe Symbol
  , limit :: Int
  , fraction :: Fraction a
  , parameters :: params
  , strategy :: params -> Strategy ExchangeRate a
  }


outputDir :: String
outputDir = "output/"

refreshAccount ::
  (Num a, Fractional a, TS.Length (TS.TimeseriesRaw price a), Functor price, StripPrice price) =>
  Fraction a -> Strategy price a -> Account a -> TS.TimeseriesRaw price a -> Account a
refreshAccount (Fraction f) (Strategy stgy) acnt@(Account bc qc) ts =
  let ms = stgy ts
      signal = lastSignal ms
      (_, prc) = stripPrice (TS.last ts)
  in case signal of
       Buy -> Account (bc - f*bc) (qc + f*bc*prc)
       Sell -> Account (bc + qc/prc) 0
       None -> acnt


trader ::
  (Show a, Read a, Fractional a, ToFileString params) =>
  MVar (ExchangeRate (UTCTime, HM.HashMap Symbol a)) -> BarLength -> Config params a -> IO ()
trader mvar bl cfg = do

  now <- getCurrentTime
  
  let sym = symbol cfg

      fileName =
        outputDir
        ++ show sym
        ++ "-" ++ toFileString bl
        ++ "-" ++ toFileString (parameters cfg)
        ++ "-" ++ formatTime defaultTimeLocale (iso8601DateFormat (Just "%X%Z")) now
        ++ ".csv"

  hd <- Sys.openFile fileName Sys.WriteMode
  Sys.hSetBuffering hd Sys.LineBuffering

  putStrLn ("Starting trader for " ++ fileName)

  let -- empty = TS.TimeseriesRaw (show sym) (Price Vec.empty)

      tsReq = (Binance.defaultRequestParams "" sym bl) {
        Binance.limit = Just (limit cfg)
        }
      
      refreshAcc = refreshAccount (fraction cfg) ((strategy cfg) (parameters cfg))
      
  Just empty <- Binance.getSymbol tsReq

  let loop us@(acnt, zs) = do
        hm <- takeMVar mvar

        let distr (ExchangeRate (Just x)) = Just (ExchangeRate x)
            distr _ = Nothing
          
            prc = distr (fmap (sequence . fmap (HM.lookup sym)) hm)
            bcurr = distr (fmap (join . flip fmap (usdtSymbol cfg) . flip HM.lookup . snd) hm)

        case fmap (TS.addLast zs) prc of
          Just as -> do
            let newAcnt = refreshAcc acnt as
                bc@(ExchangeRate (t, _)) = TS.last as
                loggerData = LoggerData {
                  ltime = t
                  , lbaseCurrencyUSDT = bcurr                        
                  , lcurrencyPair = fmap snd bc
                  , laccount = newAcnt
                  }
            
            logger hd loggerData
            loop (newAcnt, as)
          Nothing -> loop us

  loop (account cfg, empty)



ticker ::
  (Show a, Read a, Fractional a, ToFileString params) =>
  (BarLength, [Config params a]) -> IO ()
ticker (bl, cfgs) = do
  ts <- nextTimeSlices bl

  let g c = do
        mvar <- newEmptyMVar
        void (forkIO (trader mvar bl c))
        return mvar

  mvars <- mapM g cfgs

  let f t = do     
        now <- getCurrentTime
        let delta = t `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))

        ps <- Binance.getTickerExchangeRate Binance.defaultExchangeRateRequest

        xnow <- getCurrentTime

        let msg =
              "Received ticker for [ "
              ++ List.intercalate ", " (map (show . symbol) cfgs)
              ++ " ] at "
              ++ show xnow
        
        putStrLn msg
        
        mapM_ (flip putMVar ps) mvars

  mapM_ f ts


start ::
  (Show a, Read a, Fractional a, ToFileString params) =>
  [(BarLength, [Config params a])] -> IO ()
start xs = do
  let io = mapM_ (forkIO . ticker) xs
  io
  threadDelay (60*1000*1000*1000)
