{-# LANGUAGE ScopedTypeVariables #-}


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
import FinancialTimeseries.Type.Types (Price(..))
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
  , strategy :: params -> Strategy a
  }


outputDir :: String
outputDir = "output/"

refreshAccount ::
  (Num a, Fractional a) =>
  Fraction a -> Strategy a -> Account a -> TS.TimeseriesRaw a -> Account a
refreshAccount (Fraction f) (Strategy stgy) acnt@(Account bc qc) ts =
  let ms = stgy ts
      signal = lastSignal ms
      Price (_, prc) = TS.last ts
  in case signal of
       Buy -> Account (bc - f*bc) (qc + f*bc*prc)
       Sell -> Account (bc + qc/prc) 0
       None -> acnt


trader ::
  (Show a, Read a, Fractional a, ToFileString params) =>
  MVar (Price (UTCTime, HM.HashMap Symbol a)) -> BarLength -> Config params a -> IO ()
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

        let distr (Price (Just p)) = Just (Price p)
            distr _ = Nothing   

            price = distr (fmap (sequence . fmap (HM.lookup sym)) hm)
            bcurr = distr (fmap (join . flip fmap (usdtSymbol cfg) . flip HM.lookup . snd) hm)

        case fmap (TS.addLast zs) price of
          Just as -> do
            let newAcnt = refreshAcc acnt as
                bc@(Price (t, _)) = TS.last as
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
        forkIO (trader mvar bl c)
        return mvar

  mvars <- mapM g cfgs

  let f t = do     
        now <- getCurrentTime
        let delta = t `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))

        ps <- Binance.getTickerPrice Binance.defaultPriceRequest

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
  {-
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  takeMVar mvar
  -}





  
 {-
trader :: MVar (Price (UTCTime, HM.HashMap Symbol a)) -> Config params a -> IO ()
trader m cfg = do
  let tsReq = (Binance.defaultRequestParams "" (symbol cfg) (barLength cfg)) {
        Binance.limit = Just (limit cfg)
        }
        
  Just ts <- Binance.getSymbol tsReq
  
  let req = Binance.defaultPriceRequest {
        Binance.priceSymbol = Just (symbol cfg)
        }


      loop as = do
        m <- takeMVar m
        
        
  in loop ts
-}


{-
ticker :: Config params Double -> IO ()
ticker cfg = do

  mvar <- newEmptyMVar

  t <- getCurrentTime
    
  let dts = BarLength.toNominalDiffTime (barLength cfg)

      zs = map ((`addUTCTime` t) . (*dts)) [0, 1 ..]

      req = Binance.defaultPriceRequest {
        Binance.priceSymbol = Just (symbol cfg)
        }

      f acc z = do
        
        now <- getCurrentTime
        let delta = z `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))

        avg <- Binance.getTickerPrice req
        
        let price =
              case fmap (sequence . fmap (HM.lookup (symbol cfg))) avg of
                Price (Just p) -> Just (Price p)
                _ -> Nothing
                
        case fmap (TS.addLast acc) price of
          Just as -> do
            putMVar mvar as
            return as
          _ -> return acc
          
  foldM_ f undefined zs

-}


{-

start :: Config params Double -> IO ()
start cfg = do

  let req = (Binance.defaultRequestParams "" (symbol cfg) (barLength cfg)) {
        Binance.limit = Just (limit cfg)
        }

  Just ts <- Binance.getSymbol req

  Sys.hSetBuffering Sys.stdout Sys.LineBuffering
  
  let Price (t, _) = TS.last ts
      
      dts = BarLength.toNominalDiffTime (barLength cfg)

      zs = map ((`addUTCTime` t) . (*dts)) [0, 1 ..]

      req = Binance.defaultPriceRequest {
        Binance.priceSymbol = Just (symbol cfg)
        }

      refreshAcc = refreshAccount (fraction cfg) ((strategy cfg) (parameters cfg))

      f (acnt, acc) z = do
        
        now <- getCurrentTime
        let delta = z `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))

        avg <- Binance.getTickerPrice req
  
        let price =
              case fmap (sequence . fmap (HM.lookup (symbol cfg))) avg of
                Price (Just p) -> Just (Price p)
                _ -> Nothing

        case fmap (TS.addLast acc) price of
          Just as -> do
            let newAcnt = refreshAcc acnt as
                Price (t0, p0) = TS.last as            
            putStrLn
              (show t0 ++ "," ++ show p0 ++ "," ++ show (baseCurrency newAcnt) ++ "," ++ show (quoteCurrency newAcnt))
            return (newAcnt, as)
          Nothing -> return (acnt, acc)
  
  foldM_ f (account cfg, ts) zs

-}
