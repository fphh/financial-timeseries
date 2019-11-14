


module FinancialTimeseries.Source.Binance.BarLength where

import Data.Time (NominalDiffTime)

import qualified Data.List as List

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

data BarLength =
  Min Int
  | Hour Int
  | Day Int
  | Week Int
  | Month Int
  deriving (Show, Eq)

instance Pretty BarLength where
  pretty bl =
    let bs =
          [ (Min 1, "1m"), (Min 3, "3m"), (Min 5, "5m"), (Min 15, "15m"), (Min 30, "30m")
          , (Hour 1, "1h"), (Hour 2, "2h"), (Hour 4, "4h"), (Hour 6, "6h"), (Hour 8, "8h"), (Hour 12, "12h")
          , (Day 1, "1d"), (Day 3, "3d")
          , (Week 1, "1w"), (Month 1, "1M") ]
        err = error $
          "binance does not support interval '" ++ show bl ++ "'\n"
          ++ "Supported intervals are:\n" ++ List.intercalate "\n" (map (('\t':) . show . fst) bs)
    in maybe err id (List.lookup bl bs)



toNominalDiffTime :: BarLength -> NominalDiffTime
toNominalDiffTime bl = realToFrac $
  case bl of
    Min m -> 60*m
    Hour h -> 60*60*h
    Day d -> 24*60*60*d
    Week w -> 7*24*60*60*w
    Month m -> 30*7*24*60*60*m
