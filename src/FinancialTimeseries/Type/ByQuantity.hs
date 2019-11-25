{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.ByQuantity where


import FinancialTimeseries.Type.Types (Price(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

data ByQuantity a = ByQuantity {
  byQuantity :: a
  , quantity :: Double
  } deriving (Show, Read, Eq, Functor)


-- Rather use type composition ???
newtype PriceByQuantity x = PriceByQuantity {
  unPriceByQuantity :: Price (ByQuantity x)
  } deriving (Show, Read, Eq, Functor)

newtype ExchangeRateByQuantity x = ExchangeRateByQuantity {
  unExchangeRateByQuantity :: ExchangeRate (ByQuantity x)
  } deriving (Show, Read, Eq, Functor)
