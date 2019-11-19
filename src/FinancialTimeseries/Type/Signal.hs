{-# LANGUAGE FlexibleContexts #-}

module FinancialTimeseries.Type.Signal where

import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..))
import qualified FinancialTimeseries.Type.Timeseries as TS



data Signal =
  Buy
  | Sell
  | None
  deriving (Show)


lastSignal ::
  (TS.Length (TS.TimeseriesRaw price a)) =>
  TS.Timeseries price a -> Signal
lastSignal ms =
  let k =
        case TS.investedSegments ms of
          [] -> Nothing
          ss -> Just (last ss)
          
      ls = TS.lastSegment ms
      lastIndex = TS.length (TS.timeseriesRaw ms) - 1

  in case (k, ls) of
       (_, Just (HalfSegment j)) | lastIndex == j -> Buy
       (Just (Segment _ j), _) | lastIndex == j -> Sell
       _ -> None


