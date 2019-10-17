{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Test where

import qualified Test.QuickCheck as QC

import Data.Time (UTCTime, addUTCTime, parseTimeM, defaultTimeLocale)

import qualified Data.Vector as Vec


import qualified Data.List as List

import FinancialTimeseries.Algorithm.MovingAverage (Window(..), movingAverage)

import FinancialTimeseries.Type.Type.Invested (Invested(..), NotInvested(..), partitionInvested)
import FinancialTimeseries.Type.Type.Equity (Equity(..))
import FinancialTimeseries.Type.Type.Yield (Yield(..))
import FinancialTimeseries.Type.Evaluate (Long(..), long, evaluateInvested)
import FinancialTimeseries.Type.Segment (segments, Segment(..))
import FinancialTimeseries.Type.Timeseries (Timeseries(..), slice)



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
      , timeseries = us
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
  && check_segment_last_idx_prop (Vec.length ts - 1) ss
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
  let slc = slice ts
      f (Left (NotInvested _)) (Right (Invested _)) = True
      f (Right (Invested _)) (Left (NotInvested _)) = True
      f _ _ = False
  in and (zipWith f slc (tail slc))
  
-- --------------------------------------------------------------------------

prop_slice :: TS -> Bool
prop_slice (TS ts@(Timeseries _ as ss _)) =
  let segs = segments ss
      slc = slice ts
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
      Invested inv = snd slc
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
    TS us <- QC.arbitrary `QC.suchThat` ((>0) . Vec.length . timeseries . unTS)
    let len = Vec.length (timeseries us)
    m <- QC.choose (1, len)
    return $ MovingAvgTest {
      window = Window m
      , mavgTs = us
      }
      
check_moving_avg_props :: MovingAvgTest -> Bool
check_moving_avg_props (MovingAvgTest (Window m) ts) =
  m > 0
  && Vec.length (timeseries ts) >= m

prop_moving_avg_timeseries_props :: MovingAvgTest -> Bool
prop_moving_avg_timeseries_props (MovingAvgTest m ts) =
  let us = movingAverage m ts
  in check_timeseries_prop us

prop_moving_avg_length :: MovingAvgTest -> Bool
prop_moving_avg_length (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ ss _ ((_, as):_) = movingAverage w ts
  in Vec.length as == Vec.length ss - m + 1

prop_moving_avg_alignment :: MovingAvgTest -> Bool
prop_moving_avg_alignment (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ ss _ ((_, as):_) = movingAverage w ts
      zs = Vec.drop (m-1) ss
  in Vec.map fst as == Vec.map fst zs

prop_moving_avg_segment_indices :: MovingAvgTest -> Bool
prop_moving_avg_segment_indices (MovingAvgTest w@(Window m) ts) =
  let Timeseries _ ss segs ((_, as):_) = movingAverage w ts
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
  
