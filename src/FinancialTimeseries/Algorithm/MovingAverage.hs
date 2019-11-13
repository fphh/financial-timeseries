

module FinancialTimeseries.Algorithm.MovingAverage where

import qualified Data.Vector as Vec

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), Timeseries(..))
import FinancialTimeseries.Type.Types (Price(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

newtype Window = Window {
  unWindow :: Int
  } deriving (Show)

instance Pretty Window where
  pretty = show

data UpDown =
  Up Int
  | Down Int
  deriving (Show)

movingAverage :: (Real a, Fractional a) => Window -> TimeseriesRaw a -> Timeseries a
movingAverage (Window m) ts@(TimeseriesRaw _ (Price vs)) =
  let idx = Vec.fromList [0 .. Vec.length vs - m]
  
      slc i =
        let ws = Vec.slice i m vs
            (tn, xn) = Vec.last ws
        in (tn, realToFrac (Sample.mean (Vec.map (realToFrac . snd) ws)), xn)
      us = Vec.map slc idx

      f i ((_, m0, x0), (_, m1, x1)) acc
        | m0 <= x0 && m1 > x1 = Up (i+m) : acc
        | m0 > x0 && m1 <= x1 = Down (i+m) : acc
        | otherwise = acc
        
      xs = Vec.ifoldr' f [] (Vec.zip us (Vec.tail us))

      g [] = (Nothing, [])
      g [Up i] = (Just (HalfSegment i), [])
      g (Up i:Down j:zs) = fmap (Segment i j :) (g zs)
      g (Down _:zs) = g zs
      -- g (Up _:zs) = g zs
      
      h (t, s, _) = (t, s)

      (hs, isegs) = g xs
      
      res = Timeseries {
        timeseriesRaw = ts
        , investedSegments = isegs
        , lastSegment = hs
        , additionalSeries = [Labeled "Moving Average" (Price (Vec.map h us))]
        }
  in res
