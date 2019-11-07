

module FinancialTimeseries.Render.Table where

import Data.Function (on)

import qualified Data.List as List


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)


import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Table (Cell(..), Table(..), emptyCell)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)


table :: (Real a, Pretty a, Pretty params) => Table params a -> Html
table (Table ttle chs ts) =
  let us = map (\l@(Labeled _ t) -> (length t, l)) ts
      (len, _) = List.maximumBy (compare `on` fst) us

      colHeaders = emptyCell : chs
      vs =
        colHeaders
        : map (\(l, Labeled lbl t) -> CString (pretty lbl) : t ++ replicate (len - l) emptyCell) us

      cell c = H5.div ! "rTableCell" $ H5.toHtml c
      row cs = H5.div ! "rTableRow" $ mapM_ cell cs
      
      hcell c = H5.div ! "rTableHead" $ H5.toHtml c
      hrow cs = H5.div ! "rTableRow" $ mapM_ hcell cs
      
  in H5.div ! "rTable" $ do
     hrow (ttle : replicate len "")
     mapM_ (row . map pretty) vs

