

module FinancialTimeseries.Render.Table where


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)


import FinancialTimeseries.Render.Css ((!))




table :: String -> [[String]] -> Html
table title ts =
  let cell c = H5.div ! "rTableCell" $ H5.toHtml c
      row cs = H5.div ! "rTableRow" $ mapM_ cell cs
      
      hcell c = H5.div ! "rTableHead" $ H5.toHtml c
      hrow cs = H5.div ! "rTableRow" $ mapM_ hcell cs
      
  in H5.div ! "rTable" $ do
     hrow [title, ""]
     mapM_ row ts
