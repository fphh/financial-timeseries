

module FinancialTimeseries.Type.Fraction where

newtype Fraction a = Fraction {
  unFraction :: a
  } deriving (Show)
