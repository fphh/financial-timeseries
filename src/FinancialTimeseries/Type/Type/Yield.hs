{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Type.Yield where


import FinancialTimeseries.Util.Pretty (Pretty, pretty)
  
newtype Yield a = Yield {
  unYield :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (Yield a) where
  pretty (Yield a) = "Yield:\n" ++ pretty a
