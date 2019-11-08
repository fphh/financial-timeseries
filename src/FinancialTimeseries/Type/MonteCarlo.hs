{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.MonteCarlo where

import Data.Vector (Vector)

import Data.Distributive (Distributive, distribute)

import FinancialTimeseries.Util.DistributivePair (DistributivePair, distributePair, undistributePair, distPair, undistPair)

data MonteCarlo a = MonteCarlo {
  unMonteCarlo :: a
  } deriving (Show, Functor)

instance Distributive MonteCarlo where
  distribute = MonteCarlo . fmap unMonteCarlo

instance DistributivePair MonteCarlo where
  distributePair = distPair MonteCarlo unMonteCarlo
  undistributePair = undistPair MonteCarlo unMonteCarlo


data Broom a = Broom {
  unBroom :: Vector (Vector a)
  } deriving (Show)
