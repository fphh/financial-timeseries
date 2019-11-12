
module FinancialTimeseries.Source.Row where

import FinancialTimeseries.Type.Types (Price(..))


newtype Volume = Volume {
  unVolume :: Double
  } deriving (Show)

data Row = Row {
  open :: Price Double
  , high :: Price Double
  , low :: Price Double
  , close :: Price Double
  , volume :: Volume
  } deriving (Show)
  

newtype Extract a = Extract {
  unExtract :: Row -> Price a
  }
