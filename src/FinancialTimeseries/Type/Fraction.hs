

module FinancialTimeseries.Type.Fraction where

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

newtype Fraction a = Fraction {
  unFraction :: a
  } deriving (Show)


instance (Show a) => Pretty (Fraction a) where
  pretty (Fraction x) = "Fraction " ++ show x
