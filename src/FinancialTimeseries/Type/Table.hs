
module FinancialTimeseries.Type.Table where

import FinancialTimeseries.Type.Labeled (Labeled)

data Table params a = Table {
  title :: String
  , rowHeaders :: [String]
  , rows :: [Labeled params [a]]
  } deriving (Show)
