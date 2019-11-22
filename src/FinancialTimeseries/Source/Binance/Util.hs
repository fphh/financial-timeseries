
module FinancialTimeseries.Source.Binance.Util where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified Data.Text as Text
import qualified Data.Aeson as Ae

import Text.Read (readMaybe)

import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol(Symbol))



utcToMillis :: UTCTime -> Integer
utcToMillis = (1000*) . round . utcTimeToPOSIXSeconds


toNumber :: (Read a) => Ae.Value -> Maybe a
toNumber (Ae.String str) = readMaybe (Text.unpack str)
toNumber _ = Nothing


toSymbol :: Ae.Value -> Maybe Symbol
toSymbol (Ae.String str) =
  case readMaybe (Text.unpack str) of
    Nothing -> Just (Symbol (Text.unpack str))
    x -> x
toSymbol _ = Nothing
