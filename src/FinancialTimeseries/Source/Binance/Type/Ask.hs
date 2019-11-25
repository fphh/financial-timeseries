{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Source.Binance.Type.Ask where

import Control.Monad (mzero)

import qualified Data.Vector as Vec
import qualified Data.Aeson as Ae

import FinancialTimeseries.Type.ByQuantity (ByQuantity(..))
import FinancialTimeseries.Source.Binance.Util (toNumber)



newtype Ask a = Ask {
  unAsk :: a
  } deriving (Show, Read, Functor, Eq)

instance (Read a) => Ae.FromJSON (ByQuantity (Ask a)) where
  parseJSON (Ae.Array as) = do
    let prc = toNumber (as Vec.! 0)
        qty = toNumber (as Vec.! 1)

    case (prc, qty) of
      (Just p, Just q) -> return (ByQuantity (Ask p) q)
      _ -> mzero

  parseJSON _ = mzero

  
