
module FinancialTimeseries.Type.Chart where

import Data.Vector (Vector)

import FinancialTimeseries.Type.Labeled (Labeled)

data Chart params x a = Chart {
  title :: String
  , curves :: [Labeled params [Vector (x, a)]]
  } deriving (Show)
