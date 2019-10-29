{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Type.Segment where

import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..))

data Segment = Segment {
  from :: !Int
  , to :: !Int
  } deriving (Show, Read)

inv :: b -> Either a (Invested b)
inv = Right . Invested

ninv :: a -> Either (NotInvested a) b
ninv = Left . NotInvested


segments :: [Segment] -> [Either (NotInvested Segment) (Invested Segment)]
segments =
  let go [] = []
      go [s] = [inv s]
      go (s@(Segment _ b):t@(Segment c _:_)) = inv s : ninv (Segment b c) : go t
  in go
