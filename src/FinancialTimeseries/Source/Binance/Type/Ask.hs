{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Source.Binance.Type.Ask where

import Control.Monad (mzero)

import qualified Data.Vector as Vec
import qualified Data.Aeson as Ae

import FinancialTimeseries.Source.Binance.Util (toNumber)



data Ask a = Ask {
  askPrice :: a
  , askQty :: Double
  } deriving (Show, Read, Functor)

instance (Read a) => Ae.FromJSON (Ask a) where
  parseJSON (Ae.Array as) = do
    let prc = toNumber (as Vec.! 0)
        qty = toNumber (as Vec.! 1)

    case (prc, qty) of
      (Just p, Just q) -> return (Ask p q)
      _ -> mzero

  parseJSON _ = mzero

  
