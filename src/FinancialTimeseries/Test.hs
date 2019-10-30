{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Test where

import Data.Function (on)

import Control.Applicative (liftA2)

import qualified Test.QuickCheck as QC

import Data.Time (UTCTime, addUTCTime, parseTimeM, defaultTimeLocale)

import qualified Data.Vector as Vec
import Data.Vector (Vector)


import qualified Data.List as List

-- import FinancialTimeseries.Algorithm.MonteCarlo (MCStats(), statsHelper, start)
import FinancialTimeseries.Algorithm.MovingAverage (Window(..), movingAverage)
import FinancialTimeseries.Algorithm.Statistics (ROI(..), Stats(count, meanROI, totalROI), statistics)

import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..), Equity(..), Price(..), Yield(..), partitionInvested)
import FinancialTimeseries.Type.Evaluate (long, evaluateInvested)
import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Segment (segments, Segment(..))
import FinancialTimeseries.Type.Timeseries (Timeseries(..), slice)

import Debug.Trace (trace)

data ListOfSegments = ListOfSegments Int [Segment] deriving (Show)

instance QC.Arbitrary ListOfSegments where
  arbitrary = do
    xs <- QC.listOf (QC.choose (1, 4))
    start <- QC.choose (0, 3)
    end <- QC.choose (0, 3)
    let f [] = []
        f [_] = []
        f (a:b:us) = Segment a b : f us
        
        ys = f (List.scanl' (+) start xs)

    return
      $ flip ListOfSegments ys
      $ end + case List.null ys of
                True -> 0
                False -> to (List.last ys) + 1


newtype TS = TS {
  unTS :: Timeseries Double
  } deriving (Show)

instance QC.Arbitrary TS where
  arbitrary = do
    ListOfSegments len seg <- QC.arbitrary

    let Just d = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe UTCTime
        f i = fmap (\x -> (realToFrac i `addUTCTime` d, x)) (QC.choose (10, 20))

    us <- Vec.generateM len f 
        
    return $ TS $ Timeseries {
      name = "arbitrary"
      , timeseries = Price us
      , investedSegments = seg
      , additionalSeries = []
      }


timeseriesTest2 :: IO (Timeseries Double)
timeseriesTest2 = fmap (last . map unTS) (QC.sample' (QC.arbitrary :: QC.Gen TS))

-- --------------------------------------------------------------------------

check_segment_length_non_zero_prop :: [Segment] -> Bool
check_segment_length_non_zero_prop ss =
  List.null ss
  || all (\(Segment a b) -> b-a > 0) ss

prop_segment_length_non_zero :: ListOfSegments -> Bool
prop_segment_length_non_zero (ListOfSegments _ ss) =
  check_segment_length_non_zero_prop ss

-- --------------------------------------------------------------------------

check_segment_last_idx_prop :: Int -> [Segment] -> Bool
check_segment_last_idx_prop len ss =
  List.null ss
  || to (List.last ss) <= len

prop_segment_last_idx :: ListOfSegments -> Bool
prop_segment_last_idx (ListOfSegments len ss) =
  check_segment_last_idx_prop len ss

-- --------------------------------------------------------------------------

check_segment_all_less_prop :: [Segment] -> Bool
check_segment_all_less_prop ss =
  let xs = concatMap (\(Segment a b) -> [a, b]) ss
  in and (zipWith (<) xs (tail xs))
  
prop_segment_all_less :: ListOfSegments -> Bool
prop_segment_all_less (ListOfSegments _ ss) =
  check_segment_all_less_prop ss

-- --------------------------------------------------------------------------

check_timeseries_prop :: Timeseries a -> Bool
check_timeseries_prop (Timeseries _ ts ss _) =
  check_segment_length_non_zero_prop ss
  && check_segment_last_idx_prop (Vec.length (unPrice ts) - 1) ss
  && check_segment_all_less_prop ss
  
prop_timeseries :: TS -> Bool
prop_timeseries (TS ts) =
  check_timeseries_prop ts

-- --------------------------------------------------------------------------

prop_alternating_inv_ninv_segment :: TS -> Bool
prop_alternating_inv_ninv_segment (TS (Timeseries _ _ ss _)) =
  let segs = segments ss
      f (Left (NotInvested _)) (Right (Invested _)) = True
      f (Right (Invested _)) (Left (NotInvested _)) = True
      f _ _ = False
  in and (zipWith f segs (tail segs))
  
-- --------------------------------------------------------------------------

prop_alternating_inv_ninv_slice :: TS -> Bool
prop_alternating_inv_ninv_slice (TS ts) =
  let Price slc = slice ts
      f (Left (NotInvested _)) (Right (Invested _)) = True
      f (Right (Invested _)) (Left (NotInvested _)) = True
      f _ _ = False
  in and (zipWith f slc (tail slc))
  
-- --------------------------------------------------------------------------

prop_slice :: TS -> Bool
prop_slice (TS ts@(Timeseries _ (Price as) ss _)) =
  let segs = segments ss
      Price slc = slice ts
      p (Right (Invested (Segment a b))) (Right (Invested v)) =
        Vec.head v == as Vec.! a
        && Vec.last v == as Vec.! b
      p (Left (NotInvested (Segment a b))) (Left (NotInvested v)) =
        Vec.head v == as Vec.! a
        && Vec.last v == as Vec.! b
      p _ _ = False
  in and (zipWith p segs slc)

-- --------------------------------------------------------------------------

prop_evaluate :: TS -> Bool
prop_evaluate (TS ts) =
  let start = 100
      slc = partitionInvested (slice ts)
      Long (Equity (Invested res)) = evaluateInvested (Equity start) (long slc)
      f v = snd (Vec.last v) / snd (Vec.head v)
      Invested inv = snd (unPrice slc)
      res2 = start * product (map f inv)
      eps = 1.0e-9
  in case Vec.null res of
       True -> True
       False -> abs (snd (Vec.last res) - res2) < eps
  
-- --------------------------------------------------------------------------

data MovingAvgTest = MovingAvgTest {
  window :: Window
  , mavgTs :: Timeseries Double
  } deriving (Show)

instance QC.Arbitrary MovingAvgTest where
  arbitrary = do
    TS us <- QC.arbitrary `QC.suchThat` ((>0) . Vec.length . unPrice . timeseries . unTS)
    let len = Vec.length (unPrice (timeseries us))
    m <- QC.choose (1, len)
    return $ MovingAvgTest {
      window = Window m
      , mavgTs = us
      }
      
check_moving_avg_props :: MovingAvgTest -> Bool
check_moving_avg_props (MovingAvgTest (Window m) ts) =
  m > 0
  && Vec.length (unPrice (timeseries ts)) >= m

prop_moving_avg_timeseries_props :: MovingAvgTest -> Bool
prop_moving_avg_timeseries_props (MovingAvgTest m ts) =
  let us = movingAverage m ts
  in check_timeseries_prop us

prop_moving_avg_length :: MovingAvgTest -> Bool
prop_moving_avg_length (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ (Price ss) _ ((_, Price as):_) = movingAverage w ts
  in Vec.length as == Vec.length ss - m + 1

prop_moving_avg_alignment :: MovingAvgTest -> Bool
prop_moving_avg_alignment (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ (Price ss) _ ((_, Price as):_) = movingAverage w ts
      zs = Vec.drop (m-1) ss
  in Vec.map fst as == Vec.map fst zs

prop_moving_avg_segment_indices :: MovingAvgTest -> Bool
prop_moving_avg_segment_indices (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ (Price ss) segs ((_, Price as):_) = movingAverage w ts
      zs = Vec.drop (m-1) ss
      tsegs = map (\(Segment a b) -> Segment (a-m+1) (b-m+1)) segs
      
      p a =
        let (_, x0) = zs Vec.! (a-1)
            (_, y0) = as Vec.! (a-1)
            (_, x1) = zs Vec.! a
            (_, y1) = as Vec.! a
        in (x0 <= y0 && x1 > y1) || (x0 > y0 && x1 <= y1)

      
  in all (p . from) tsegs && all (p . to) tsegs

-- --------------------------------------------------------------------------

 
prop_statistics_mean :: MovingAvgTest -> Bool
prop_statistics_mean (MovingAvgTest w ts) =
  let zs = movingAverage w ts
      lg = long (partitionInvested (slice zs))
      Long (Yield (NotInvested x, Invested y)) = statistics lg
      
      eps = 1.0e-4
      
      p u v =
        abs ((unROI (meanROI u) ^ count u) - (unROI (totalROI u))) < eps
        && abs ((unROI (meanROI v) ^ count v) - (unROI (totalROI v))) < eps

  in case liftA2 p x y of
       Nothing -> True
       Just b -> b


-- --------------------------------------------------------------------------

data StatsHelper a = StatsHelper (Vector a) [Vector a] deriving (Show)

instance QC.Arbitrary a => QC.Arbitrary (Vector a) where
  arbitrary = QC.choose (1, 3) >>= fmap Vec.fromList . QC.vector

instance (QC.Arbitrary a) => QC.Arbitrary (StatsHelper a) where
  arbitrary = do
    x <- QC.arbitrary
    liftA2 StatsHelper (fmap (Vec.cons x) QC.arbitrary) (fmap (map (Vec.cons x)) QC.arbitrary)

{-

prop_montecarlo_peak_drawdown :: StatsHelper (QC.Positive Double) -> Bool
prop_montecarlo_peak_drawdown (StatsHelper v vs) =
  let ws = map (Vec.map QC.getPositive) (v:vs)
      stats = statsHelper ws
      (hmx, mx) = List.maximumBy (compare `on` snd) (map (\w -> (Vec.head w, Vec.maximum w)) ws)
      (hmi, mi) = List.minimumBy (compare `on` snd) (map (\w -> (Vec.head w, Vec.minimum w)) ws)
      eps = 1.0e-7
  in abs (hmx * maxPeak stats - mx) < eps
     && abs (hmi * maxDrawdown stats - mi) < eps


prop_montecarlo_mean_profit :: StatsHelper (QC.Positive Double) -> Bool
prop_montecarlo_mean_profit (StatsHelper v vs) =
  let ws = map (Vec.map QC.getPositive) (v:vs)
      m = sum (map (log . Vec.last) ws) / fromIntegral (length ws)
      stats = statsHelper ws
      s = start stats
      eps = 1.0e-9
  in abs (meanProfit stats * s - exp m) < eps
-}

-- --------------------------------------------------------------------------

check :: (QC.Testable a) => a -> IO ()
check = QC.quickCheck . QC.withMaxSuccess 10000
 
test :: IO ()
test = do
  
  check prop_segment_length_non_zero
  check prop_segment_last_idx
  check prop_segment_all_less
  check prop_timeseries
  check prop_alternating_inv_ninv_segment
  check prop_alternating_inv_ninv_slice
  check prop_slice
  check prop_evaluate

  check prop_moving_avg_timeseries_props
  check prop_moving_avg_length
  check prop_moving_avg_alignment
  check prop_moving_avg_segment_indices

  check prop_statistics_mean


  -- check prop_montecarlo_peak_drawdown
  -- check prop_montecarlo_mean_profit
