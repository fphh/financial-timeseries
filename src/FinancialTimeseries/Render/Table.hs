

module FinancialTimeseries.Render.Table where

import Data.Function (on)

import qualified Data.List as List


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)


import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Type.Table (Table(..))


table :: Table -> Html
table (Table ttle ts) =
  let us = map (\t -> (length t, t)) ts
      (len, _) = List.maximumBy (compare `on` fst) us
      vs = map (\(l, t) -> t ++ replicate (len - l) "") us

      cell c = H5.div ! "rTableCell" $ H5.toHtml c
      row cs = H5.div ! "rTableRow" $ mapM_ cell cs
      
      hcell c = H5.div ! "rTableHead" $ H5.toHtml c
      hrow cs = H5.div ! "rTableRow" $ mapM_ hcell cs
      
  in H5.div ! "rTable" $ do
     hrow (ttle : replicate (len-1) "")
     mapM_ row vs
