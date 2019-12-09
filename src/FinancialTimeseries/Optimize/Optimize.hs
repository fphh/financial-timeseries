

module FinancialTimeseries.Optimize.Optimize where

import Data.Function (on)
import Data.Distributive (Distributive)

import qualified Data.List as List

import qualified Data.Vector as Vec

import Data.Maybe (catMaybes)

import qualified Statistics.Test.StudentT as StudentT
import qualified Statistics.Test.Types as TTypes
import qualified Statistics.Types as Types


import FinancialTimeseries.Algorithm.Evaluate (Profit, long)

import FinancialTimeseries.Statistics.Statistics (tradeStatistics, trMeanYield, moments, sampleSize, yield)

import FinancialTimeseries.Type.Long (Long(..))
import qualified FinancialTimeseries.Type.Strategy as Strategy
import FinancialTimeseries.Type.Strategy (Strategy(Strategy))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, slice)
import FinancialTimeseries.Type.Types (TradeYield(..), Invested(..), partitionInvested)


data Metrics a = Metrics {
  name :: String
  , metrics :: a
  } deriving (Show, Eq, Ord)

data Config optParams price a = Config {
  strategy :: [Strategy optParams price a]
  , evaluationMetrics :: Vec.Vector a -> Metrics a
  }


timeseriesYield :: (Real a, Floating a) => Vec.Vector a -> Metrics a
timeseriesYield vs =
  let stats = tradeStatistics vs
  in Metrics {
    name = "timeseriesYield"
    , metrics = trMeanYield (moments stats) ** fromIntegral (sampleSize stats)
    }
  
tradeMeanYield :: (Real a, Floating a) => Vec.Vector a -> Metrics a
tradeMeanYield vs =
  let stats = tradeStatistics vs
  in Metrics {
    name = "tradeMeanYield"
    , metrics = trMeanYield (moments stats)
    }

evalStrategy ::
  (Distributive price, Profit price, Fractional a) =>
  TimeseriesRaw price a
  -> Strategy optParams price a
  -> Long (TradeYield (Invested (Vec.Vector a)))
evalStrategy ts (Strategy _ ps stgy) =
  let t = stgy ps ts
      lg = long (partitionInvested (slice t))
      k = fmap (Vec.fromList . map (yield . Vec.map snd))
  in fmap (fmap (snd . fmap k)) lg


equalDistribution ::
  (Real a) =>
  Int -> Types.PValue Double -> Long (TradeYield (Invested (Vec.Vector a))) -> Bool
equalDistribution n pval (Long (TradeYield (Invested vs))) =
  let x = ceiling ((fromIntegral (Vec.length vs) / fromIntegral n) :: Double)
  
      f xs | Vec.length xs == 0 = []
      f xs = chunk xs
      
      chunk = (\(as, bs) -> as : f bs) . Vec.splitAt x
      cs = chunk (Vec.map realToFrac vs)
      
      ds = catMaybes (zipWith (StudentT.studentTTest TTypes.SamplesDiffer) cs (tail cs))
      es = map (TTypes.isSignificant pval) ds

  in all (==TTypes.NotSignificant) es


optimize ::
  (Distributive price, Profit price, Fractional a, Real a, Floating a) =>
  Config optParams price a -> TimeseriesRaw price a -> [(optParams, Metrics a)]
optimize (Config strgies metr) ts =
  let ss = map (\stgy -> (Strategy.parameters stgy, evalStrategy ts stgy)) strgies

      n = 10
      pval = Types.mkPValue 0.05

      bs = filter (equalDistribution n pval . snd) ss

      f (Long (TradeYield (Invested vs))) = metr vs
      cs = map (fmap f) bs
      
      ds = List.sortBy (compare `on` snd) cs
  in ds

