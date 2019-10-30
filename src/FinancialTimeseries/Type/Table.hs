
module FinancialTimeseries.Type.Table where


data Table a = Table {
  title :: String
  , header :: [String]
  , table :: [[a]]
  } deriving (Show)
