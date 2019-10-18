{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.Types where

import FinancialTimeseries.Type.Invested (Invested(..), NotInvested(..))
import FinancialTimeseries.Util.Pretty (Pretty, pretty)

newtype Equity a = Equity {
  unEquity :: a
  } deriving (Show, Functor)


instance Pretty a => Pretty (Equity a) where
  pretty (Equity a) = "Equity:\n" ++ pretty a


newtype Yield a = Yield {
  unYield :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (Yield a) where
  pretty (Yield a) = "Yield:\n" ++ pretty a


newtype Price a = Price {
  unPrice :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (Price a) where
  pretty (Price a) = "Price:\n" ++ pretty a


swapYieldInvested ::
  Yield (NotInvested a, Invested a)
  -> (NotInvested (Yield a), Invested (Yield a))
swapYieldInvested (Yield (NotInvested x, Invested y)) =
  (NotInvested (Yield x), Invested (Yield y))

swapInvestedEquity ::
  (NotInvested (Equity a), Invested (Equity a))
  -> Equity (NotInvested a, Invested a)
swapInvestedEquity (NotInvested (Equity x), Invested (Equity y)) =
  Equity (NotInvested x, Invested y)
