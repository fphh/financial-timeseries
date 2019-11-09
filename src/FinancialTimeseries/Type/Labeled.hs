{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Labeled where


data Labeled label content = Labeled {
  label :: label
  , content :: content
  } deriving (Show, Read, Functor)
