{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Histogram where


newtype Histogram a = Histogram {
  unHistogram :: a
  } deriving (Functor, Show)
