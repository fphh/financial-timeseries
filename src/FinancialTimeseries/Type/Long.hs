{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.Long where

import Data.Distributive (Distributive, distribute)

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

newtype Long a = Long {
  unLong :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Long a) where
  pretty (Long x) = "Long\n" ++ pretty x

instance Distributive Long where
  distribute = Long . fmap unLong
