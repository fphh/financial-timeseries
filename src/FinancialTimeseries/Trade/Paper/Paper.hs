{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FinancialTimeseries.Trade.Paper.Paper where

import qualified System.IO as Sys

import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Control.Monad (void, join, liftM, liftM2)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT)

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, formatTime, defaultTimeLocale, iso8601DateFormat)

import qualified FinancialTimeseries.Source.Binance.Klines as Klines
import qualified FinancialTimeseries.Source.Binance.OrderBook as OrderBook

import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength, nextTimeSlices)
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Trade.LoggerData (LoggerData(..), logger)
import FinancialTimeseries.Trade.TradeReaderIO (TradeReaderIO, runTradeReaderIO, fileNamePrefix)
import qualified FinancialTimeseries.Trade.TradeReaderIO as TRIO
import FinancialTimeseries.Type.ByQuantity (ByQuantity(..))
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Signal (Signal(..), lastSignal)
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Types (StripPrice, ExchangeRate(..))
import qualified FinancialTimeseries.Type.Timeseries as TS

import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)



data Config params a = Config {
  account :: Account a
  , symbol :: Symbol
  , limit :: Int
  , fraction :: Fraction a
  , orderBookDepth :: OrderBook.Limit
  , strategy :: Strategy params ExchangeRate a
  }

-- type Message a = Either () (ExchangeRate (UTCTime, (Bid a, Ask a)))

data Message a =
  Message (ExchangeRate (UTCTime, (Bid a, Ask a)))
  | End

refreshAccount ::
  (Num a, Fractional a, TS.Length (TS.TimeseriesRaw price a), Functor price, StripPrice price) =>
  Fraction a
  -> Strategy optParams price a
  -> ExchangeRate (Bid a, Ask a)
  -> Account a
  -> TS.TimeseriesRaw price a
  -> Account a
refreshAccount (Fraction f) (Strategy _ ps stgy) bidAsk acnt@(Account bc qc) ts =
  let ms = stgy ps ts
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



trader ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  MVar (Message a) -> BarLength -> Config params a -> TradeReaderIO ()
trader mvar bl cfg = do
  let sym = symbol cfg

  filePref <- fileNamePrefix sym bl (strategy cfg)
  
  let fileNameCsv = filePref ++ ".csv"
      fileNameParams = filePref ++ ".params"

  liftIO $ do

    writeFile fileNameParams (show (name (strategy cfg), bl, parameters (strategy cfg)))
    hd <- Sys.openFile fileNameCsv Sys.WriteMode
    Sys.hSetBuffering hd Sys.LineBuffering
    
    putStrLn ("Starting trader for " ++ fileNameCsv)

    let tsReq = (Klines.defaultQuery sym bl) {
          Klines.limit = Just (limit cfg + 100)
          }
      
        refreshAcc = refreshAccount (fraction cfg) (strategy cfg)

        newData acnt bidAsk = do
        
          Just as <- Klines.get tsReq

          let -- prc = fmap (fmap (\(Bid b, Ask a) -> (a+b)/2)) bidAsk
        
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

        -- end = const (return ())

        loop acnt = takeMVar mvar >>=
          \x -> case x of
                  End -> return ()
                  Message y -> newData acnt y

          -- either end (newData acnt)

        
    loop (account cfg)



ticker ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  (BarLength, [(Config params a,  MVar (Message a))]) -> TradeReaderIO ()
ticker (bl, mcfgs) = do
  ts <- liftIO (nextTimeSlices bl)

  let finally mvar filePath = do
        print filePath
        putMVar mvar End
        
      g (c, mvar) = mapReaderT (flip forkFinally (finally mvar)) (trader mvar bl c)

  mapM_ g mcfgs

  let f (cfg, mvar) = liftIO $ do
        let sym = symbol cfg
            depth = orderBookDepth cfg

        ob <- OrderBook.get (OrderBook.defaultQuery sym depth)

        let bidAsk = fmap (fmap byQuantity) (OrderBook.exchangeRateByQuantity 10 ob)
        
        putMVar mvar (Message bidAsk)

      fs = map (forkIO . f) mcfgs
 
      h t = liftIO $ do
        now <- getCurrentTime
        let delta = t `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000 :: Double))
        sequence_ fs
        xnow <- getCurrentTime
        putStrLn ("Received ticker for " ++ show (map (symbol . fst) mcfgs) ++ " at " ++ show xnow)
        
  mapM_ h ts


  
wait16hours :: IO ()
wait16hours = threadDelay (1000*1000*60*60*16)

waitNminute :: Int -> TradeReaderIO ()
waitNminute n = liftIO (threadDelay (1000*1000*60*n))

wait1minute :: TradeReaderIO ()
wait1minute = waitNminute 1


addMVars ::
  [(BarLength, [Config params a])]
  -> TradeReaderIO [(BarLength, [(Config params a, MVar (Message a))])]
addMVars xs = liftIO $ do
  let g x = sequence (x, newEmptyMVar)
  mapM (sequence . fmap (mapM g)) xs


sendEnd :: [(BarLength, [(Config params a, MVar (Message a))])] -> TradeReaderIO ()
sendEnd xs = liftIO $ do
  let g (_, mvar) = do
        putMVar mvar End
        void (takeMVar mvar)
        
  mapM_ (sequence . fmap (mapM g)) xs  


start ::
  (Show a, Read a, Fractional a, ToFileString params, Show params) =>
  [(BarLength, [Config params a])] -> TradeReaderIO ()
start xs = do
  cs <- addMVars xs

  mapM_ (mapReaderT forkIO . ticker) cs
  waitNminute (4*60)
  sendEnd cs

  let g bl cfg = fileNamePrefix (symbol cfg) bl (strategy cfg)
      f (bl, cs) = sequence (map (g bl) cs)

  fs <- sequence (map f xs)

  liftIO $ do
    putStrLn "after sendEnd"
    print fs

  
