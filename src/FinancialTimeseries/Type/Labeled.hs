{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Labeled where

import Data.Distributive (Distributive, distribute)

data Labeled params label = Labeled {
  parameters :: params
  , label :: label
  } deriving (Show, Functor)
