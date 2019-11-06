{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Types where

import Data.Bifunctor (bimap)
import Data.Distributive (Distributive, distribute)
import Data.Either (partitionEithers)

import FinancialTimeseries.Util.DistributivePair (DistributivePair, distributePair, undistributePair, distPair, undistPair)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)



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

instance DistributivePair Equity where
  distributePair = distPair Equity unEquity
  undistributePair = undistPair Equity unEquity


newtype TimeseriesYield a = TimeseriesYield {
  unTimeseriesYield :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (TimeseriesYield a) where
  pretty (TimeseriesYield x) = "TimeseriesYield:\n" ++ pretty x

instance Distributive TimeseriesYield where
  distribute = TimeseriesYield . fmap unTimeseriesYield

instance DistributivePair TimeseriesYield where
  distributePair = distPair TimeseriesYield unTimeseriesYield
  undistributePair = undistPair TimeseriesYield unTimeseriesYield


newtype TradeYield a = TradeYield {
  unTradeYield :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (TradeYield a) where
  pretty (TradeYield x) = "TradeYield:\n" ++ pretty x

instance Distributive TradeYield where
  distribute = TradeYield . fmap unTradeYield

instance DistributivePair TradeYield where
  distributePair = distPair TradeYield unTradeYield
  undistributePair = undistPair TradeYield unTradeYield


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
  Price [Either (NotInvested a) (Invested b)] -> Price (NotInvested [a], Invested [b])
partitionInvested =
  fmap (bimap (NotInvested . map unNotInvested) (Invested . map unInvested) . partitionEithers)

invested :: Price [Either (NotInvested a) (Invested a)] -> Price (Invested [a])
invested = fmap snd . partitionInvested

notInvested :: Price [Either (NotInvested a) (Invested a)] -> Price (NotInvested [a])
notInvested = fmap fst . partitionInvested
