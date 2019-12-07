

module FinancialTimeseries.Type.Strategy where


import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, Timeseries)
import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)


data Strategy params price a = Strategy {
  name :: String
  , parameters :: params
  , freeStrategy :: params -> TimeseriesRaw price a -> Timeseries price a
  }


instance (ToFileString params) => ToFileString (Strategy params price a) where
  toFileString (Strategy n ps _) = n ++ "-" ++ toFileString ps
