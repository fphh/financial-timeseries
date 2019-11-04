{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Util.Pretty where

import Data.Time (UTCTime)

import qualified Data.List as List


import qualified Data.Text.Lazy as Text
import Data.String (fromString)
import Formatting (format, (%), fixed)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty = id

instance Show a => Pretty [Vector (UTCTime, a)] where
  pretty = List.intercalate "\n" . map pretty

instance Show a => Pretty (Vector (UTCTime, a)) where
  pretty = Vec.foldr (\(t, x) -> ((show t ++ "\t" ++ show x ++ "\n") ++)) ""

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "\n(\n" ++ pretty x ++ "\n,\n" ++ pretty y ++ "\n)\n"



perc :: (Num a, Real a) => a -> String
perc = Text.unpack . format (fixed 2 % fromString "%") . (100*)

fmt :: (Num a, Real a) => a -> String
fmt = Text.unpack . format (fixed 8)

