{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Type.Pretty where

import Data.Time (UTCTime)

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty = List.intercalate "\n" . map pretty


instance Show a => Pretty (Vector (UTCTime, a)) where
  pretty = Vec.foldr (\(t, x) -> ((show t ++ "\t" ++ show x ++ "\n") ++)) ""


instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "\n(\n" ++ pretty x ++ "\n,\n" ++ pretty y ++ "\n)\n"

