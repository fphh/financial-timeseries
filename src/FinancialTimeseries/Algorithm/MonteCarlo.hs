{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified System.Random as R

import qualified Statistics.Sample as Sample

import qualified Data.Text.Lazy as Text
import Data.String (fromString)

import Formatting (format, (%), fixed)

import FinancialTimeseries.Type.Evaluate (Evaluate, evaluate)
import FinancialTimeseries.Type.Table (Table(..))
import FinancialTimeseries.Type.Types (Equity(..), Yield(..), Invested(..), NotInvested(..))
import FinancialTimeseries.Util.Util (biliftA)



sample ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  longOrShort (Yield (notInv [Vector u], inv [Vector u]))
  -> [Int]
  -> [Int]
  -> longOrShort (Yield (notInv [Vector u], inv [Vector u]))
sample xs as bs =
  let g rs us =
        let zs = Vec.fromList us
            len = Vec.length zs
        in map ((zs Vec.!) . (`mod` len)) rs
        
  in fmap (fmap (biliftA (fmap (g as)) (fmap (g bs)))) xs


data Config = Config {
  sampleLength :: Int
  , numberOfSamples :: Int
  } deriving (Show)

samples ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  Int
  -> longOrShort (Yield (notInv [Vector (t, a)], inv [Vector (t, a)]))
  -> IO [longOrShort (Yield (notInv [Vector (t, a)], inv [Vector (t, a)]))]
samples sampLen xs = do
  (ga, gb) <- fmap R.split R.newStdGen

  let chunk = (\(us, vs) -> us : chunk vs) . splitAt sampLen
      as = chunk (R.randoms ga)
      bs = chunk (R.randoms gb)

  return (zipWith (sample xs) as bs)


data MonteCarlo a = MonteCarlo {
  mcNotInv :: NotInvested a -- (Vector a)
  , mcInv :: Invested a -- (Vector a)
  } deriving (Show, Functor)


mc ::
  (Num a, Functor longOrShort, Evaluate longOrShort, Distributive longOrShort) =>
  Config
  -> Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (longOrShort (MonteCarlo (Vector (Vector a))))
mc cfg eqty xs = do
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = biliftA k k . unzip
  return (fmap (uncurry MonteCarlo . h . unEquity) ws)


-- perc :: (Num a, Real a) => a -> String
-- perc = Text.unpack . format (fixed 2 % fromString "%") . (100*)

fmt :: (Num a, Real a) => a -> String
fmt = Text.unpack . format (fixed 8)

class Row a where
  row :: a -> [String] 

data Quantiles a = Quantiles {
  q05 :: !a
  , q25 :: !a
  , q50 :: !a
  , q75 :: !a
  , q95 :: !a
  } deriving (Show)

instance (Real a) => Row (Quantiles a) where
  row (Quantiles a b c d e) = map fmt [a, b, c, d, e]

data Probabilities a = Probabilities {
  p0'50 :: !a
  , p0'75 :: !a
  , p1'00 :: !a
  , p1'25 :: !a
  , p1'50 :: !a
  , p1'75 :: !a
  , p2'00 :: !a
  } deriving (Show)

instance (Real a) => Row (Probabilities a) where
  row (Probabilities a b c d e f g) = map fmt [a, b, c, d, e, f, g]

data Moments a = Moments {
  maxYield :: !a
  , minYield :: !a
  , meanYield :: !a
  , stdDevYield :: !a
  } deriving (Show)
  
instance (Real a) => Row (Moments a) where
  row (Moments a b c d) = map fmt [a, b, c, d]

data Stats a = Stats {
  quantiles :: Quantiles a
  , probabilities :: Probabilities a
  , moments :: Moments a
  , pdf :: Vector (Double, a)
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
        , meanYield = realToFrac $ Sample.mean sortedFrac
        , stdDevYield = realToFrac $ Sample.stdDev sortedFrac
        }
        
  in Stats {
    quantiles = q
    , probabilities = p
    , moments = m
    , pdf = Vec.imap (\i x -> (fromIntegral i / noe, x)) sorted
    }


stats2list :: (Real a) => [Stats a] -> [Table]
stats2list xs =
  let qheaders = ["Q05", "Q25", "Q50", "Q75", "Q95"]
      pheaders = ["P(X < 0.5)", "P(X < 0.75)", "P(X < 1.0)", "P(X < 1.25)", "P(X < 1.5)", "P(X < 1.75)", "P(X < 2.0)"]
      mheaders = ["Max.", "Min.", "Mean", "StdDev."]
  in Table "Quantiles" (qheaders : map (row . quantiles) xs)
     : Table "Probabilities" (pheaders : map (row . probabilities) xs)
     : Table "Moments" (mheaders : map (row . moments) xs)
     : []



mcYields ::
  (Fractional a) =>
  MonteCarlo (Vector (Vector a)) -> Yield (MonteCarlo (Vector a))
mcYields = Yield . fmap (Vec.map (\vs -> Vec.last vs / Vec.head vs))


mc2stats ::
  (Fractional a, Real a, Functor f) =>
  f (MonteCarlo (Vector a)) -> f (MonteCarlo (Stats a))
mc2stats = fmap (fmap mkStatistics)

stats2table ::
  (Functor f, Real a) =>
  f (MonteCarlo [Stats a]) -> f (MonteCarlo [Table])
stats2table = fmap (fmap stats2list)

{-
stats2list :: (Show a, Real a) => Stats a -> [[String]]
stats2list stats =  (\(as, bs) -> [as, bs]) $ unzip $
  ("Q25", fmt (q25 stats))
  : ("Q50 (Median)", fmt (q50 stats))
  : ("Q75", fmt (q75 stats))
  : ("P(X < 1)", fmt (pleqOne stats))
  : ("Max.", fmt (maxProfit stats))
  : ("Min.", fmt (minProfit stats))
  : ("Mean", fmt (meanProfit stats))
  : ("StdDev.", fmt (stdDevProfit stats))
  : []

data MCStats a = MCStats {
  count :: !Int
  , winners :: !Int
  , loosers :: !Int
  , start :: !a
  , maxLength :: !Int
  , minLength :: !Int
  , meanLength :: !Double
  , stdDevLength :: !Double
  , profit :: Stats a
  , absoluteDrawdown :: Stats a
  , relativeDrawdown :: Stats a
  } deriving (Show)

mcStatsHelper ::
  (Ord a, Num a, Fractional a, Real a, Floating a) =>
  Vector (Vector a) -> MCStats a
mcStatsHelper vs =
  let strt = Vec.head (Vec.head vs)
      ls = Vec.map Vec.last vs
      (win, loose) = Vec.partition (1.0 <) ls
      lens = Vec.map Vec.length vs
      lensFrac = Vec.map realToFrac lens
      absDDs = Vec.map Vec.minimum vs
      relDDs = Vec.map (\v -> Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v))) vs
  in MCStats {
    count = Vec.length vs
    , winners = Vec.length win
    , loosers = Vec.length loose
    , start = strt
    , maxLength = Vec.maximum lens
    , minLength = Vec.minimum lens
    , meanLength = Sample.mean lensFrac
    , stdDevLength = Sample.stdDev lensFrac
    , profit = statsHelper (Vec.map (/strt) ls)
    , absoluteDrawdown = statsHelper (Vec.map (/strt) absDDs)
    , relativeDrawdown = statsHelper relDDs
    }

mcStats2list :: (Show a) => MCStats a -> [[String]]
mcStats2list stats = (\(as, bs) -> [as, bs]) $ unzip $
  ("Count", show (count stats))
  : ("Winners", show (winners stats))
  : ("Loosers", show (loosers stats))
  : ("Start", show (start stats))
  : ("Max. Len.", show (maxLength stats))
  : ("Min. Len.", show (minLength stats))
  : ("Mean Len.", fmt (meanLength stats))
  : ("StdDev. Len.", fmt (stdDevLength stats))
  : []

stats ::
  (Ord a, Real a, Fractional a, Floating a) =>
  MonteCarlo a -> (NotInvested (MCStats a), Invested (MCStats a))
stats (MonteCarlo a b) = (fmap mcStatsHelper a, fmap mcStatsHelper b)

-}
