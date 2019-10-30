{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.MonteCarlo where

newtype MonteCarlo a = MonteCarlo {
  unMonteCarlo :: a
  } deriving (Show, Functor)
