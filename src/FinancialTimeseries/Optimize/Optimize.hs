

module FinancialTimeseries.Optimize.Optimize where

import Data.Function (on)

import Data.Bifunctor (bimap)

import qualified Data.List as List

import qualified Data.Vector as Vec

import Data.Maybe (catMaybes)

import qualified Statistics.Test.StudentT as StudentT
import qualified Statistics.Test.Types as TTypes
import qualified Statistics.Types as Types


import FinancialTimeseries.Algorithm.Evaluate (long)

import FinancialTimeseries.Statistics.Statistics (tradeStatistics, trMeanYield, moments, sampleSize, yield)

import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), slice)
import FinancialTimeseries.Type.Types (TimeseriesYield(..), TradeYield(..), Invested(..), partitionInvested)



data OptimizeConfig optParams a = OptimizeConfig {
  strategy :: optParams -> Strategy a
  , params :: [optParams]
  }


evalStrategy ::
  (Fractional a) =>
  TimeseriesRaw a
  -> Strategy a
  -> Long (TradeYield (Invested (Vec.Vector a)))
evalStrategy ts stgy =
  let t = unStrategy stgy ts
      lg' = long (partitionInvested (slice t))

      lg =
        let k = fmap (map (Vec.map (fmap (1/))))
        in fmap (fmap (bimap k k)) lg'
      
      
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
  (Fractional a, Real a, Floating a) =>
  OptimizeConfig optParams a -> TimeseriesRaw a -> [(optParams, TimeseriesYield a)]
optimize (OptimizeConfig strgy ps) ts =
  let ss = map (\p -> (p, evalStrategy ts (strgy p))) ps

      n = 10
      pval = Types.mkPValue 0.05

      bs = filter (equalDistribution n pval . snd) ss

      f (Long (TradeYield (Invested vs))) =
        let stats = tradeStatistics vs
        in TimeseriesYield (trMeanYield (moments stats) ** fromIntegral (sampleSize stats))
      cs = map (fmap f) bs
      
      ds = List.sortBy (compare `on` snd) cs
  in ds
