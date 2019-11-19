
module FinancialTimeseries.Source.Row where


newtype Volume a = Volume {
  unVolume :: a
  } deriving (Show)

data Row price a = Row {
  open :: price a
  , high :: price a
  , low :: price a
  , close :: price a
  , volume :: Volume a
  } deriving (Show)
  

newtype Extract price a = Extract {
  unExtract :: Row price a -> price a
  }
