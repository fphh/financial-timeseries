

module FinancialTimeseries.Trade.Paper.Paper where

import qualified System.IO as Sys

import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)


import qualified Data.Vector as Vec

import qualified FinancialTimeseries.Algorithm.MovingAverage as MA

import qualified FinancialTimeseries.Source.Binance.BarLength as BarLength
import FinancialTimeseries.Source.Binance.BarLength (BarLength)
import qualified FinancialTimeseries.Source.Binance.Binance as Binance
import FinancialTimeseries.Source.Binance.Symbol (Symbol)

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Types (Price(..))
import qualified FinancialTimeseries.Type.Timeseries as TS

{-
sym = Symbol.ETH Symbol.XMRETH
len = BarLength.Min 1
win = MA.Window 3 -- 33
frac = Fraction 1.0
-}

data Account a = Account {
  baseCurrency :: a
  , quoteCurrency :: a
  } deriving (Show)

data Config params a = Config {
  account :: Account a
  , symbol :: Symbol
  , barLength :: BarLength
  , limit :: Int
  , fraction :: Fraction a
  , parameters :: params
  , strategy :: params -> Strategy a
  }

refresh ::
  (Num a, Fractional a) =>
  Fraction a -> Strategy a -> Account a -> TS.TimeseriesRaw a -> Account a
refresh (Fraction f) (Strategy stgy) acnt@(Account bc qc) ts =
  let ms = stgy ts
      Segment _ k = last (TS.investedSegments ms)
      ls = TS.lastSegment ms
      Price (_, prc) = TS.last ts
      lastIndex = TS.length ts - 1
  in case (k, ls) of
       (_, Just (HalfSegment j))
         | lastIndex == j ->
             let newBC = bc - f*bc
                 newQC = qc + f*bc*prc
             in Account newBC newQC
       (j, _)
         | lastIndex == j ->
             let newBC = bc + qc/prc
                 newQC = 0
             in Account newBC newQC
       _ -> acnt
                  


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

      req = Binance.defaultAvgPriceRequest (symbol cfg)

      refreshAccount = refresh (fraction cfg) ((strategy cfg) (parameters cfg))

      f (acnt, acc) z = do
        
        now <- getCurrentTime
        let delta = z `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000))

        avg <- Binance.getAvgPrice req

        case fmap (TS.addLast acc) avg of
          Just as -> do
            let newAcnt = refreshAccount acnt as
                Price (t0, p0) = TS.last as            
            putStrLn
              (show t0 ++ "," ++ show p0 ++ "," ++ show (baseCurrency newAcnt) ++ "," ++ show (quoteCurrency newAcnt))
            return (newAcnt, as)
          Nothing -> return (acnt, acc)
  
  foldM_ f (account cfg, ts) zs

