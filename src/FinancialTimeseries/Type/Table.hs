
module FinancialTimeseries.Type.Table where


data Table = Table {
  title :: String
  , table :: [[String]]
  } deriving (Show)
