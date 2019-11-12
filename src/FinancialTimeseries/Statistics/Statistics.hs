

module FinancialTimeseries.Statistics.Statistics where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.Chart (Chart(..), LChart)
import FinancialTimeseries.Type.Labeled (Labeled)
import FinancialTimeseries.Type.Table (Table(..), Cell(..), Row, row)



data Quantiles a = Quantiles {
  q05 :: a
  , q25 :: a
  , q50 :: a
  , q75 :: a
  , q95 :: a
  } deriving (Show)

instance Row Quantiles where
  row (Quantiles a b c d e) = map Cell [a, b, c, d, e]

data Probabilities a = Probabilities {
  p0'50 :: a
  , p0'75 :: a
  , p1'00 :: a
  , p1'25 :: a
  , p1'50 :: a
  , p1'75 :: a
  , p2'00 :: a
  } deriving (Show)

instance Row Probabilities where
  row (Probabilities a b c d e f g) = map Cell [a, b, c, d, e, f, g]

data TradeMoments a = TradeMoments {
  trMaxYield :: a
  , trMinYield :: a
  , trMeanYield :: a
  , trStdDevYield :: a
  } deriving (Show)
  
instance Row TradeMoments where
  row (TradeMoments a b c d) = map Cell [a, b, c, d]

data TimeseriesMoments a = TimeseriesMoments {
  tsMaxYield :: a
  , tsMinYield :: a
  , tsMeanYield :: a
  , tsStdDevYield :: a
  } deriving (Show)
  
instance Row TimeseriesMoments where
  row (TimeseriesMoments a b c d) = map Cell [a, b, c, d]

data Stats moments a = Stats {
  sampleSize :: Int
  , quantiles :: Quantiles a
  , probabilities :: Probabilities a
  , moments :: moments a
  , cdf :: Vector (Double, a)
  } deriving (Show)


tradeMoments ::
  (Real a, Fractional a) =>
  Vector a -> TradeMoments a
tradeMoments sorted =
  let sortedFrac = Vec.map realToFrac sorted
  in TradeMoments {
    trMaxYield = Vec.last sorted
    , trMinYield = Vec.head sorted
    , trMeanYield = realToFrac $ exp (Sample.mean (Vec.map log sortedFrac))
    , trStdDevYield = realToFrac $ exp (Sample.stdDev (Vec.map log sortedFrac))
    }
     
timeseriesMoments ::
  (Real a, Fractional a) =>
  Vector a -> TimeseriesMoments a
timeseriesMoments sorted =
  let sortedFrac = Vec.map realToFrac sorted
  in TimeseriesMoments {
    tsMaxYield = Vec.last sorted
    , tsMinYield = Vec.head sorted
    , tsMeanYield = realToFrac $ Sample.mean sortedFrac
    , tsStdDevYield = realToFrac $ Sample.stdDev sortedFrac
    }

     
stats2list ::
  (Row moments) =>
  [Labeled params (Stats moments a)] -> [Table params a]
stats2list xs =
  let qheaders = map CString ["Q05", "Q25", "Q50", "Q75", "Q95", "Sample Size"]
      pheaders = map CString ["P(X < 0.5)", "P(X < 0.75)", "P(X < 1.0)", "P(X < 1.25)", "P(X < 1.5)", "P(X < 1.75)", "P(X < 2.0)", "Sample Size"]
      mheaders = map CString ["Max.", "Min.", "Mean", "StdDev.", "Sample Size"]
      mkRow g x = row (g x) ++ [CInt (sampleSize x)]
  in Table "Quantiles" qheaders (map (fmap (mkRow quantiles)) xs)
     : Table "Moments" mheaders (map (fmap (mkRow moments)) xs)
     : Table "Probabilities" pheaders (map (fmap (mkRow probabilities)) xs)
     : []

stats2cdfChart :: [Labeled params (Stats moments a)] -> LChart params Double a
stats2cdfChart = Chart "CDF" . map (fmap ((:[]) . cdf))


statisticsWithMoments ::
  (Ord a, Num a, Fractional a, Real a) =>
  (Vector a -> moments a) -> Vector a -> Stats moments a
statisticsWithMoments mms vs =
  let noe = fromIntegral (Vec.length vs)
      sorted = Vec.modify Merge.sort vs

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

  in Stats {
    sampleSize = Vec.length vs
    , quantiles = q
    , probabilities = p
    , moments = mms sorted
    , cdf = Vec.imap (\i x -> (fromIntegral i / noe, x)) sorted
    }


timeseriesStatistics ::
  (Real a, Fractional a) =>
  Vector a -> Stats TimeseriesMoments a
timeseriesStatistics = statisticsWithMoments timeseriesMoments

tradeStatistics ::
  (Real a, Fractional a) =>
  Vector a -> Stats TradeMoments a
tradeStatistics = statisticsWithMoments tradeMoments

yield :: (Fractional a) => Vector a -> a
yield v = Vec.last v / Vec.head v

absoluteDrawdown :: (Ord a, Fractional a) => Vector a -> a
absoluteDrawdown v = Vec.minimum v / Vec.head v

relativeDrawdown :: (Ord a, Num a, Fractional a) => Vector a -> a
relativeDrawdown v = Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v))


