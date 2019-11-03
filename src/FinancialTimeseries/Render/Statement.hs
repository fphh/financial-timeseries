
module FinancialTimeseries.Render.Statement where


import Data.Time (UTCTime)

import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import FinancialTimeseries.Render.HtmlReader (HtmlReader)


statement :: String -> HtmlReader Html
statement = return . H5.p . H5.toHtml . Text.pack

currentTime :: UTCTime -> HtmlReader Html
currentTime t = return $ H5.p $ do
  H5.toHtml (Text.pack "Current time:")
  H5.b $ H5.toHtml (Text.pack (show t))

