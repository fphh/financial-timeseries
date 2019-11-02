{-# LANGUAGE RankNTypes #-}


module FinancialTimeseries.Util.DistributivePair where

import FinancialTimeseries.Util.Util (biliftA)

distPair ::
  (forall x. x -> f x) -> (forall x. f x -> x) -> f (a, b) -> (f a, f b)
distPair unUn un = biliftA unUn unUn . un

undistPair ::
  (forall x. x -> f x) -> (forall x. f x -> x) -> (f a, f b) -> f (a, b)
undistPair unUn un = unUn . biliftA un un

class DistributivePair f where
  distributePair :: f (a, b) -> (f a, f b)
  undistributePair :: (f a, f b) -> f (a, b)
