
module FinancialTimeseries.Source.Row where

import FinancialTimeseries.Type.Types (Price(..))


newtype Volume a = Volume {
  unVolume :: a
  } deriving (Show)

data Row a = Row {
  open :: Price a
  , high :: Price a
  , low :: Price a
  , close :: Price a
  , volume :: Volume a
  } deriving (Show)
  

newtype Extract a = Extract {
  unExtract :: Row a -> Price a
  }
