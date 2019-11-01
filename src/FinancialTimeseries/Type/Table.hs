
module FinancialTimeseries.Type.Table where

import FinancialTimeseries.Type.Labeled (Labeled)

data Table params a = Table {
  title :: String
  , rowHeader :: [String]
  , table :: [Labeled params [a]]
  } deriving (Show)
