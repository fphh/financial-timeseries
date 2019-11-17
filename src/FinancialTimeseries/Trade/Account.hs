

module FinancialTimeseries.Trade.Account where



data Account a = Account {
  baseCurrency :: a
  , quoteCurrency :: a
  } deriving (Show, Read)


