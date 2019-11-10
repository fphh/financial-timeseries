

module FinancialTimeseries.Statistics.Statistics where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.Table (Cell(..), Row, row)



data Quantiles a = Quantiles {
  q05 :: !a
  , q25 :: !a
  , q50 :: !a
  , q75 :: !a
  , q95 :: !a
  } deriving (Show)

instance Row Quantiles where
  row (Quantiles a b c d e) = map Cell [a, b, c, d, e]

data Probabilities a = Probabilities {
  p0'50 :: !a
  , p0'75 :: !a
  , p1'00 :: !a
  , p1'25 :: !a
  , p1'50 :: !a
  , p1'75 :: !a
  , p2'00 :: !a
  } deriving (Show)

instance Row Probabilities where
  row (Probabilities a b c d e f g) = map Cell [a, b, c, d, e, f, g]

data Moments a = Moments {
  maxYield :: !a
  , minYield :: !a
  , meanYield :: !a
  , stdDevYield :: !a
  } deriving (Show)
  
instance Row Moments where
  row (Moments a b c d) = map Cell [a, b, c, d]

data Stats a = Stats {
  sampleSize :: Int
  , quantiles :: Quantiles a
  , probabilities :: Probabilities a
  , moments :: Moments a
  , cdf :: Vector (Double, a)
  } deriving (Show)

mkStatistics ::
  (Ord a, Num a, Fractional a, Real a) =>
  Vector a -> Stats a
mkStatistics vs =
  let noe = fromIntegral (Vec.length vs)
      sorted = Vec.modify Merge.sort vs
      sortedFrac = Vec.map realToFrac sorted

      quart s = sorted Vec.! (round (s * noe :: Double))
      q = Quantiles {
        q05 = quart 0.05
        , q25 = quart 0.25
        , q50 = quart 0.50
        , q75 = quart 0.75
        , q95 = quart 0.95
        }

      prob s = fromIntegral (Vec.length (Vec.takeWhile (<s) sorted)) / noe
      p = Probabilities {
        p0'50 = prob 0.5
        , p0'75 = prob 0.75
        , p1'00 = prob 1.0
        , p1'25 = prob 1.25
        , p1'50 = prob 1.5
        , p1'75 = prob 1.75
        , p2'00 = prob 2.00
        }

      m = Moments {
        maxYield = Vec.last sorted
        , minYield = Vec.head sorted
        {-
        , meanYield = realToFrac $ exp (Sample.mean (Vec.map log sortedFrac))
        , stdDevYield = realToFrac $ exp (Sample.stdDev (Vec.map log sortedFrac))
-}
        , meanYield = realToFrac $ Sample.mean sortedFrac
        , stdDevYield = realToFrac $ Sample.stdDev sortedFrac
        }
        
  in Stats {
    sampleSize = Vec.length vs
    , quantiles = q
    , probabilities = p
    , moments = m
    , cdf = Vec.imap (\i x -> (fromIntegral i / noe, x)) sorted
    }


yield :: (Fractional a) => Vector a -> a
yield v = Vec.last v / Vec.head v

absoluteDrawdown :: (Ord a, Fractional a) => Vector a -> a
absoluteDrawdown v = Vec.minimum v / Vec.head v

relativeDrawdown :: (Ord a, Num a, Fractional a) => Vector a -> a
relativeDrawdown v = Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v))
