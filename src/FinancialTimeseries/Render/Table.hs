

module FinancialTimeseries.Render.Table where

import Data.Function (on)

import qualified Data.List as List


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)


import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Type.Table (Table(..))
import FinancialTimeseries.Util.Pretty (fmt)


table :: (Real a) => Table a -> Html
table (Table ttle chs rhs ts) =
  let rhsXs = rhs ++ replicate (length ts - length rhs) ""

      us = map (\t -> (length t, t)) ts
      (len, _) = List.maximumBy (compare `on` fst) us
      vs = ("" : chs) : zipWith (\r (l, t) -> r : map fmt t ++ replicate (len - l - 1) "") rhsXs us

      cell c = H5.div ! "rTableCell" $ H5.toHtml c
      row cs = H5.div ! "rTableRow" $ mapM_ cell cs
      
      hcell c = H5.div ! "rTableHead" $ H5.toHtml c
      hrow cs = H5.div ! "rTableRow" $ mapM_ hcell cs
      
  in H5.div ! "rTable" $ do
     hrow (ttle : replicate len "")
     mapM_ row vs

