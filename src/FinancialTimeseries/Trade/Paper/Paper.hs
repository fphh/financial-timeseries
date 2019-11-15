

module FinancialTimeseries.Trade.Paper.Paper where

import qualified System.IO as Sys

import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec

import qualified Data.HashMap.Strict as HM

import qualified FinancialTimeseries.Algorithm.MovingAverage as MA

import qualified FinancialTimeseries.Source.Binance.BarLength as BarLength
import FinancialTimeseries.Source.Binance.BarLength (BarLength)
import qualified FinancialTimeseries.Source.Binance.Binance as Binance
import FinancialTimeseries.Source.Binance.Symbol (Symbol)

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Signal (Signal(..), lastSignal)
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
      signal = lastSignal ms
      Price (_, prc) = TS.last ts
  in case signal of
       Buy -> Account (bc - f*bc) (qc + f*bc*prc)
       Sell -> Account (bc + qc/prc) 0
       None -> acnt


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

      refreshAccount = refresh (fraction cfg) ((strategy cfg) (parameters cfg))

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
            let newAcnt = refreshAccount acnt as
                Price (t0, p0) = TS.last as            
            putStrLn
              (show t0 ++ "," ++ show p0 ++ "," ++ show (baseCurrency newAcnt) ++ "," ++ show (quoteCurrency newAcnt))
            return (newAcnt, as)
          Nothing -> return (acnt, acc)
  
  foldM_ f (account cfg, ts) zs

