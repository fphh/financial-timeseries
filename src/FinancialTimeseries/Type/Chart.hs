
module FinancialTimeseries.Type.Chart where

import Data.Vector (Vector)

import FinancialTimeseries.Type.Labeled (Labeled)

data Chart params a = Chart {
  title :: String
  , curves :: [Labeled params (Vector (Double, a))]
  } deriving (Show)
