{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified System.Random as R

import FinancialTimeseries.Type.Evaluate (Evaluate, Long(..), {- Short(..), -} evaluate)
import FinancialTimeseries.Type.Types (Equity(..), Yield(..), Invested(..), NotInvested(..))
import FinancialTimeseries.Util.Util (biliftA)


sample ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  longOrShort (Yield (notInv [Vector u], inv [Vector u]))
  -> [Int]
  -> [Int]
  -> longOrShort (Yield (notInv [Vector u], inv [Vector u]))
sample xs as bs =
  let g rs xs =
        let zs = Vec.fromList xs
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
  mcNotInv :: NotInvested [Vector a]
  , mcInv :: Invested [Vector a]
  }

mc ::
  (Num a, Functor longOrShort, Evaluate longOrShort, Distributive longOrShort) =>
  Equity a
  -> Config
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (longOrShort (MonteCarlo a))
mc eqty cfg xs = do
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      h = biliftA distribute distribute . unzip
  return (fmap (uncurry MonteCarlo . h . unEquity) ws)
