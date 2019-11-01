
module FinancialTimeseries.Type.Table where


data Table a = Table {
  title :: String
  , columnHeader :: [String]
  , rowHeader :: [String]
  , table :: [[a]]
  } deriving (Show)
