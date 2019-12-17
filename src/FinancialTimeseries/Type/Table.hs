
module FinancialTimeseries.Type.Table where

import FinancialTimeseries.Type.Labeled (Labeled)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)

data Cell a =
  Cell a
  | CInt Int
  | CDouble Double
  | CString String
  deriving (Show)

emptyCell :: Cell a
emptyCell = CString ""

instance (Pretty a) => Pretty (Cell a) where
  pretty (Cell u) = pretty u
  pretty (CInt i) = show i
  pretty (CDouble x) = pretty x
  pretty (CString s) = s

      
data Table params a = Table {
  title :: String
  , rowHeaders :: [Cell a]
  , rows :: [Labeled params [Cell a]]
  } deriving (Show)


class Row a where
  row :: (Show x) => a x -> [Cell x]
