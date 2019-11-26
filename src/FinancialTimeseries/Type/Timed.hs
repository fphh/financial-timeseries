{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Timed where

import Data.Time (UTCTime, diffUTCTime, addUTCTime)

import FinancialTimeseries.Util.Pretty (Pretty, pretty)

data Timed a = Timed {
  start :: UTCTime
  , end :: UTCTime
  , timed :: a
  } deriving (Show, Eq, Functor)

instance (Pretty a) => Pretty (Timed a) where
  pretty (Timed s e xs) =
    "From " ++ show s ++ " to " ++ show e ++ ":\n" ++ pretty xs

middle :: Timed a -> UTCTime
middle (Timed s e _) = ((e `diffUTCTime` s) / 2) `addUTCTime` s
