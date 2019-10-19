
module FinancialTimeseries.Render.Statement where


import Data.Time (UTCTime)

import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import FinancialTimeseries.Render.Css ((!))


statement :: String -> Html
statement txt = H5.p $ H5.toHtml (Text.pack txt)


currentTime :: UTCTime -> Html
currentTime t =
  H5.p $ do
  H5.toHtml (Text.pack "Current time:")
  H5.b $ H5.toHtml (Text.pack (show t))
