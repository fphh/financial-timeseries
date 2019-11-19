

module FinancialTimeseries.Type.Strategy where


import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, Timeseries)


newtype Strategy price a = Strategy {
  unStrategy :: TimeseriesRaw price a -> Timeseries price a
  }
