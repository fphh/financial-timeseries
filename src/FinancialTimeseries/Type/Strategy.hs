

module FinancialTimeseries.Type.Strategy where


import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, Timeseries)


newtype Strategy a = Strategy {
  unStrategy :: TimeseriesRaw a -> Timeseries a
  }
