{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Types where


import Data.Distributive (Distributive, distribute)


import Data.Either (partitionEithers)

import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.Util (biliftA)



newtype Invested a = Invested {
  unInvested :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Invested a) where
  pretty (Invested x) = "Invested\n" ++ pretty x

instance Distributive Invested where
  distribute = Invested . fmap unInvested


newtype NotInvested a = NotInvested {
  unNotInvested :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (NotInvested a) where  
  pretty (NotInvested x) = "Not Invested\n" ++ pretty x

instance Distributive NotInvested where
  distribute = NotInvested . fmap unNotInvested


newtype Equity a = Equity {
  unEquity :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Equity a) where
  pretty (Equity a) = "Equity:\n" ++ pretty a

instance Distributive Equity where
  distribute = Equity . fmap unEquity


newtype Yield a = Yield {
  unYield :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Yield a) where
  pretty (Yield a) = "Yield:\n" ++ pretty a

instance Distributive Yield where
  distribute = Yield . fmap unYield


newtype Price a = Price {
  unPrice :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (Price a) where
  pretty (Price a) = "Price:\n" ++ pretty a

instance Distributive Price where
  distribute = Price . fmap unPrice


swapYieldInvested ::
  (Functor notInv, Functor inv) =>
  Yield (notInv a, inv a)
  -> (notInv (Yield a), inv (Yield a))
swapYieldInvested = biliftA (fmap Yield) (fmap Yield) . unYield


swapInvestedEquity ::
  (Functor notInv, Functor inv) =>
  (notInv (Equity a), inv (Equity a))
  -> Equity (notInv a, inv a)
swapInvestedEquity = Equity . biliftA (fmap unEquity) (fmap unEquity)


partitionInvested ::
  Price [Either (NotInvested a) (Invested b)] -> Price (NotInvested [a], Invested [b])
partitionInvested =
  fmap (biliftA (NotInvested . map unNotInvested) (Invested . map unInvested) . partitionEithers)

invested :: Price [Either (NotInvested a) (Invested a)] -> Price (Invested [a])
invested = fmap snd . partitionInvested

notInvested :: Price [Either (NotInvested a) (Invested a)] -> Price (NotInvested [a])
notInvested = fmap fst . partitionInvested
