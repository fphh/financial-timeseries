

module FinancialTimeseries.Util.Row where


class Row a where
  row :: a x -> [x]
