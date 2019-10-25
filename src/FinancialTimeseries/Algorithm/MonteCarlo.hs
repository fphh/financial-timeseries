{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified System.Random as R

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.Evaluate (Evaluate, evaluate)
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
  mcNotInv :: NotInvested (Vector (Vector a))
  , mcInv :: Invested (Vector (Vector a))
  } deriving (Show)

mc ::
  (Num a, Functor longOrShort, Evaluate longOrShort, Distributive longOrShort) =>
  Config
  -> Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (longOrShort (MonteCarlo a))
mc cfg eqty xs = do
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = biliftA k k . unzip
  return (fmap (uncurry MonteCarlo . h . unEquity) ws)


data Stats a = Stats {
  q25 :: !a
  , q50 :: !a
  , q75 :: !a
  , pleqOne :: !a
  , maxProfit :: !a
  , minProfit :: !a
  , meanProfit :: !a
  , stdDevProfit :: !a
  , pdf :: [(Double, a)]
  } deriving (Show)

statsHelper ::
  (Ord a, Num a, Fractional a, Real a, Floating a) =>
  a -> Vector a -> Stats a
statsHelper strt vs =
  let noe = fromIntegral (Vec.length vs)
      sorted = Vec.modify Merge.sort (Vec.map (/strt) vs)
      sortedFrac = Vec.map realToFrac sorted
  in Stats {
    q25 = sorted Vec.! (round (1 * (noe / 4)))
    , q50 = sorted Vec.! (round (2 * (noe / 4)))
    , q75 = sorted Vec.! (round (3 * (noe / 4)))
    , pleqOne = fromIntegral (Vec.length (Vec.takeWhile (<1) sorted)) / noe
    , maxProfit = Vec.last sorted
    , minProfit = Vec.head sorted
    , meanProfit = realToFrac $ Sample.mean sortedFrac
    , stdDevProfit = realToFrac $ Sample.stdDev sortedFrac
    , pdf = Vec.toList (Vec.imap (\i p -> (fromIntegral i / noe, p)) sorted)
    }

stats2list :: (Show a) => Stats a -> [[String]]
stats2list stats =  (\(as, bs) -> [as, bs]) $ unzip $
  ("Q25", show (q25 stats))
  : ("Q50 (Median)", show (q50 stats))
  : ("Q75", show (q75 stats))
  : ("P(X < 1)", show (pleqOne stats))
  : ("Max.", show (maxProfit stats))
  : ("Min.", show (minProfit stats))
  : ("Mean", show (meanProfit stats))
  : ("StdDev.", show (stdDevProfit stats))
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
  in MCStats {
    count = Vec.length vs
    , winners = Vec.length win
    , loosers = Vec.length loose
    , start = strt
    , maxLength = Vec.maximum lens
    , minLength = Vec.minimum lens
    , meanLength = Sample.mean lensFrac
    , stdDevLength = Sample.stdDev lensFrac
    , profit = statsHelper strt ls
    }

mcStats2list :: (Show a) => MCStats a -> [[String]]
mcStats2list stats = (\(as, bs) -> [as, bs]) $ unzip $
  ("Count", show (count stats))
  : ("Winners", show (winners stats))
  : ("Loosers", show (loosers stats))
  : ("Start", show (start stats))
  : ("Max. Len.", show (maxLength stats))
  : ("Min. Len.", show (minLength stats))
  : ("Mean Len.", show (meanLength stats))
  : ("StdDev. Len.", show (stdDevLength stats))
  : []

stats ::
  (Ord a, Real a, Fractional a, Floating a) =>
  MonteCarlo a -> (NotInvested (MCStats a), Invested (MCStats a))
stats (MonteCarlo a b) = (fmap mcStatsHelper a, fmap mcStatsHelper b)
