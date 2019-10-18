{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FinancialTimeseries.Type.Types where

import Data.Either (partitionEithers)

import Text.Printf (PrintfArg)

import qualified Graphics.Rendering.Chart.Easy as E

import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.Util (biliftA)



newtype Invested a = Invested {
  unInvested :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Invested a) where
  pretty (Invested x) = "Invested\n" ++ pretty x



newtype NotInvested a = NotInvested {
  unNotInvested :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (NotInvested a) where  
  pretty (NotInvested x) = "Not Invested\n" ++ pretty x



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

partitionInvested ::
  Price [Either (NotInvested a) (Invested b)] -> Price (NotInvested [a], Invested [b])
partitionInvested =
  fmap (biliftA (NotInvested . map unNotInvested) (Invested . map unInvested) . partitionEithers)

invested :: Price [Either (NotInvested a) (Invested a)] -> Price (Invested [a])
invested = fmap snd . partitionInvested

notInvested :: Price [Either (NotInvested a) (Invested a)] -> Price (NotInvested [a])
notInvested = fmap fst . partitionInvested
