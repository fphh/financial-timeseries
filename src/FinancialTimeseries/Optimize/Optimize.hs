

module FinancialTimeseries.Optimize.Optimize where

import Data.Function (on)

import Data.Bifunctor (bimap)

import qualified Data.List as List

import qualified Data.Vector as Vec

import Data.Maybe (catMaybes)

import qualified Statistics.Sample as Sample
import qualified Statistics.Test.StudentT as StudentT
import qualified Statistics.Test.Types as TTypes
import qualified Statistics.Types as Types


import FinancialTimeseries.Algorithm.Evaluate (long)

import FinancialTimeseries.Statistics.Statistics (yield)

import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), slice)
import FinancialTimeseries.Type.Table (Cell(..), Row, row)
import FinancialTimeseries.Type.Types (TimeseriesYield(..), TradeYield(..), NotInvested(..), Invested(..), partitionInvested)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)

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
      lg = long (partitionInvested (slice t))
      k = fmap (Vec.fromList . map (yield . Vec.map snd))
  in fmap (fmap (snd . fmap k)) lg


equalDistribution ::
  (Real a) =>
  Int -> Types.PValue Double -> Long (TradeYield (Invested (Vec.Vector a))) -> Bool
equalDistribution n pval (Long (TradeYield (Invested vs))) =
  let x = ceiling (fromIntegral (Vec.length vs) / fromIntegral n)
  
      f xs | Vec.length xs == 0 = []
      f xs = chunk xs
      
      chunk = (\(as, bs) -> as : f bs) . Vec.splitAt x
      cs = chunk (Vec.map realToFrac vs)
      
      ds = catMaybes (zipWith (StudentT.studentTTest TTypes.SamplesDiffer) cs (tail cs))
      es = map (TTypes.isSignificant pval) ds

  in all (==TTypes.NotSignificant) es


optimize ::
  (Fractional a, Real a) =>
  OptimizeConfig optParams a -> TimeseriesRaw a -> [(optParams, TimeseriesYield a)]
optimize (OptimizeConfig strgy ps) ts =
  let ss = map (\p -> (p, evalStrategy ts (strgy p))) ps

      n = 3
      pval = Types.mkPValue 0.05

      bs = filter (equalDistribution n pval . snd) ss

      f (Long (TradeYield (Invested vs))) = TimeseriesYield {- (Vec.product vs) -} (Vec.sum vs / fromIntegral (Vec.length vs))
      cs = map (fmap f) bs
      
      ds = List.sortBy (compare `on` snd) cs
  in ds
