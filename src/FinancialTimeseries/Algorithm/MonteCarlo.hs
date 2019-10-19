

module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Time (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified System.Random as R

import FinancialTimeseries.Type.Types (Yield(..))
import FinancialTimeseries.Util.Util (biliftA)


sample ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))
  -> [Int]
  -> [Int]
  -> longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))
sample xs as bs =
  let g rs xs =
        let zs = Vec.fromList xs
            len = Vec.length zs
        in map ((zs Vec.!) . (`mod` len)) rs
        
  in fmap (fmap (biliftA (fmap (g as)) (fmap (g bs)))) xs


newtype SampleLength = SampleLength {
  unSampleLength :: Int
  } deriving (Show)

samples ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  SampleLength
  -> longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))
  -> IO [longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))]
samples (SampleLength n) xs = do
  (ga, gb) <- fmap R.split R.newStdGen

  let chunk = (\(us, vs) -> us : chunk vs) . splitAt n
      as = chunk (R.randoms ga)
      bs = chunk (R.randoms gb)
      
  return (zipWith (sample xs) as bs)


newtype MonteCarlo a = MonteCarlo {
  unMonteCarlo :: Equity [Vector (UTCTime, a)]
  } deriving (Show)

newtype MonteCarloLength = MonteCarloLength {
  unMonteCarloLength :: Int
  } deriving (Show)



monteCarlo ::
  MonteCarloLength
  -> SampleLength
  -> longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))
  -> IO (MonteCarlo a)
