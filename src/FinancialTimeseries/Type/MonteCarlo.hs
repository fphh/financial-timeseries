{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.MonteCarlo where

import Data.Distributive (Distributive, distribute)

data MonteCarlo a = MonteCarlo {
  unMonteCarlo :: a
  } deriving (Show, Functor)


instance Distributive MonteCarlo where
  distribute = MonteCarlo . fmap unMonteCarlo

