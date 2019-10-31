{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}


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
  pretty (Equity x) = "Equity:\n" ++ pretty x

instance Distributive Equity where
  distribute = Equity . fmap unEquity


newtype Yield a = Yield {
  unYield :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Yield a) where
  pretty (Yield x) = "Yield:\n" ++ pretty x

instance Distributive Yield where
  distribute = Yield . fmap unYield


newtype Price a = Price {
  unPrice :: a
  } deriving (Functor, Show, Read)

instance Pretty a => Pretty (Price a) where
  pretty (Price x) = "Price:\n" ++ pretty x

instance Distributive Price where
  distribute = Price . fmap unPrice


newtype AbsoluteDrawdown a = AbsoluteDrawdown {
  unAbsoluteDrawdown :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (AbsoluteDrawdown a) where
  pretty (AbsoluteDrawdown x) = "Absolute drawdown:\n" ++ pretty x

instance Distributive AbsoluteDrawdown where
  distribute = AbsoluteDrawdown . fmap unAbsoluteDrawdown


newtype RelativeDrawdown a = RelativeDrawdown {
  unRelativeDrawdown :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (RelativeDrawdown a) where
  pretty (RelativeDrawdown x) = "Relative drawdown:\n" ++ pretty x

instance Distributive RelativeDrawdown where
  distribute = RelativeDrawdown . fmap unRelativeDrawdown

{-
distributePair ::
  (forall x. x -> f x) ->  (forall x. x -> f x) -> (forall x. f x -> x) -> f (a, b) -> (f a, f b)
distributePair unUn1 unUn2 un = biliftA unUn1 unUn1 . un
-}

swapYieldInvested ::
  (Functor notInv, Functor inv) =>
  Yield (notInv a, inv a)
  -> (notInv (Yield a), inv (Yield a))
swapYieldInvested = biliftA (fmap Yield) (fmap Yield) . unYield

unswapYieldInvested ::
  (Functor notInv, Functor inv) =>
  (notInv (Yield a), inv (Yield a))
  -> Yield (notInv a, inv a)
unswapYieldInvested = Yield . biliftA (fmap unYield) (fmap unYield)

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
