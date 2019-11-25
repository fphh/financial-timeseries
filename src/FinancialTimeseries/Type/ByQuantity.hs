{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.ByQuantity where


import FinancialTimeseries.Type.Types (Price(..))

data ByQuantity a = ByQuantity {
  byQuantity :: a
  , quantity :: Double
  } deriving (Show, Read, Eq, Ord, Functor)


-- Rather use type composition ???
newtype PriceByQuantity x = PriceByQuantity {
  unPriceByQuantity :: Price (ByQuantity x)
  }
