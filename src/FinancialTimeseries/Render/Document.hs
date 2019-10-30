
module FinancialTimeseries.Render.Document where

import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import FinancialTimeseries.Render.Css ((!))

renderDocument :: Html -> Html
renderDocument chart = do
  H5.docType
  H5.html $ do
    H5.body ! "monoFont" $ do
      H5.h1 $ H5.span $ H5.toHtml (Text.pack "Analysis")
      chart



