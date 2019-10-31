{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.MonteCarlo where

import Data.Distributive (Distributive, distribute)

newtype MonteCarlo a = MonteCarlo {
  unMonteCarlo :: a
  } deriving (Show, Functor)


instance Distributive MonteCarlo where
  distribute = MonteCarlo . fmap unMonteCarlo
