{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Types where

import Data.Bifunctor (bimap)
import Data.Distributive (Distributive, distribute)
import Data.Either (partitionEithers)

import FinancialTimeseries.Util.DistributivePair (DistributivePair, distributePair, undistributePair, distPair, undistPair)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)



newtype Invested a = Invested {
  unInvested :: a
  } deriving (Show, Functor, Eq)

instance Pretty a => Pretty (Invested a) where
  pretty (Invested x) = "Invested\n" ++ pretty x

instance Distributive Invested where
  distribute = Invested . fmap unInvested


newtype NotInvested a = NotInvested {
  unNotInvested :: a
  } deriving (Show, Functor, Eq)

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

instance DistributivePair Equity where
  distributePair = distPair Equity unEquity
  undistributePair = undistPair Equity unEquity


newtype TimeseriesYield a = TimeseriesYield {
  unTimeseriesYield :: a
  } deriving (Eq, Ord, Show, Functor)

instance Pretty a => Pretty (TimeseriesYield a) where
  pretty (TimeseriesYield x) = "TimeseriesYield:\n" ++ pretty x

instance Distributive TimeseriesYield where
  distribute = TimeseriesYield . fmap unTimeseriesYield

instance DistributivePair TimeseriesYield where
  distributePair = distPair TimeseriesYield unTimeseriesYield
  undistributePair = undistPair TimeseriesYield unTimeseriesYield


newtype TradeYield a = TradeYield {
  unTradeYield :: a
  } deriving (Eq, Ord, Show, Functor)

instance Pretty a => Pretty (TradeYield a) where
  pretty (TradeYield x) = "TradeYield:\n" ++ pretty x

instance Distributive TradeYield where
  distribute = TradeYield . fmap unTradeYield

instance DistributivePair TradeYield where
  distributePair = distPair TradeYield unTradeYield
  undistributePair = undistPair TradeYield unTradeYield


class StripPrice price where
  stripPrice :: price a -> a
  price :: a -> price a

newtype Price a = Price {
  unPrice :: a
  } deriving (Functor, Show, Read, Eq)

instance Pretty a => Pretty (Price a) where
  pretty (Price x) = "Price:\n" ++ pretty x

instance Distributive Price where
  distribute = Price . fmap unPrice

instance StripPrice Price where
  stripPrice = unPrice
  price = Price


newtype Quantity a = Quantity {
  unQuantity :: a
  } deriving (Functor, Show, Read, Eq)

instance Pretty a => Pretty (Quantity a) where
  pretty (Quantity x) = "Quantity:\n" ++ pretty x

instance Distributive Quantity where
  distribute = Quantity . fmap unQuantity


newtype ExchangeRate a = ExchangeRate {
  unExchangeRate :: a
  } deriving (Functor, Show, Read, Eq)

instance Pretty a => Pretty (ExchangeRate a) where
  pretty (ExchangeRate x) = "ExchangeRate:\n" ++ pretty x

instance Distributive ExchangeRate where
  distribute = ExchangeRate . fmap unExchangeRate

instance StripPrice ExchangeRate where
  stripPrice = unExchangeRate
  price = ExchangeRate

newtype AbsoluteDrawdown a = AbsoluteDrawdown {
  unAbsoluteDrawdown :: a
  } deriving (Functor, Show, Eq)

instance Pretty a => Pretty (AbsoluteDrawdown a) where
  pretty (AbsoluteDrawdown x) = "Absolute drawdown:\n" ++ pretty x

instance Distributive AbsoluteDrawdown where
  distribute = AbsoluteDrawdown . fmap unAbsoluteDrawdown

instance DistributivePair AbsoluteDrawdown where
  distributePair = distPair AbsoluteDrawdown unAbsoluteDrawdown
  undistributePair = undistPair AbsoluteDrawdown unAbsoluteDrawdown


newtype RelativeDrawdown a = RelativeDrawdown {
  unRelativeDrawdown :: a
  } deriving (Functor, Show)

instance Pretty a => Pretty (RelativeDrawdown a) where
  pretty (RelativeDrawdown x) = "Relative drawdown:\n" ++ pretty x

instance Distributive RelativeDrawdown where
  distribute = RelativeDrawdown . fmap unRelativeDrawdown

instance DistributivePair RelativeDrawdown where
  distributePair = distPair RelativeDrawdown unRelativeDrawdown
  undistributePair = undistPair RelativeDrawdown unRelativeDrawdown


partitionInvested ::
  (Functor price) =>
  price [Either (NotInvested a) (Invested b)] -> price (NotInvested [a], Invested [b])
partitionInvested =
  fmap (bimap (NotInvested . map unNotInvested) (Invested . map unInvested) . partitionEithers)

invested ::
  (Functor price) =>
  price [Either (NotInvested a) (Invested a)] -> price (Invested [a])
invested = fmap snd . partitionInvested

notInvested ::
  (Functor price) =>
  price [Either (NotInvested a) (Invested a)] -> price (NotInvested [a])
notInvested = fmap fst . partitionInvested
