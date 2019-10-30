{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.Short where

import Data.Distributive (Distributive, distribute)

import FinancialTimeseries.Util.Pretty (Pretty, pretty)


newtype Short a = Short {
  unShort :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Short a) where
  pretty (Short x) = "Short\n" ++ pretty x

instance Distributive Short where
  distribute = Short . fmap unShort
