{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Invested where

import Data.Either (partitionEithers)

import FinancialTimeseries.Type.Pretty (Pretty, pretty)
import FinancialTimeseries.Type.Util (biliftA)


newtype Invested a = Invested {
  unInvested :: a
  } deriving (Show, Functor)


newtype NotInvested a = NotInvested {
  unNotInvested :: a
  } deriving (Show, Functor)


instance Pretty a => Pretty (Invested a) where
  pretty (Invested x) = "Invested\n" ++ pretty x

instance Pretty a => Pretty (NotInvested a) where  
  pretty (NotInvested x) = "Not Invested\n" ++ pretty x

partitionInvested ::
  [Either (NotInvested a) (Invested b)] -> (NotInvested [a], Invested [b])
partitionInvested =
  biliftA (NotInvested . map unNotInvested) (Invested . map unInvested) . partitionEithers

invested :: [Either (NotInvested a) (Invested a)] -> Invested [a]
invested = snd . partitionInvested

notInvested :: [Either (NotInvested a) (Invested a)] -> NotInvested [a]
notInvested = fst . partitionInvested
