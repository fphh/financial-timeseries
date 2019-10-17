{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.Type.Equity where

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

newtype Equity a = Equity {
  unEquity :: a
  } deriving (Show, Functor)


instance Pretty a => Pretty (Equity a) where
  pretty (Equity a) = "Yield:\n" ++ pretty a
