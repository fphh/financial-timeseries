

module FinancialTimeseries.Render.Css where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5.Attributes as A


cssMap :: Map String H5.Attribute
cssMap = Map.fromList
  $ map (fmap (A.style . H5.stringValue))
  $ ("rTable", "display: table; background-color:#eeeeee; float: left; margin: 8px;")
  : ("rTableRow", "display: table-row;")
  : ("rTableBody", "display: table-row-group;")
  : ("rTableCell" , "display: table-cell; padding:4px; padding-left:8px; border-bottom:1px solid #aaaaaa;")
  : ("rTableHead", "display: table-cell; background-color: #ddd; width:120px; padding:4px; padding-left:8px; border-top:1px solid #aaaaaa; border-bottom:1px solid #aaaaaa; font-weight: bold;")
  : ("monoFont", "font-family: monospace;")
  : []


infixl 1 !
(!) :: (Html -> Html) -> String -> Html -> Html
a ! b = a H5.! (cssMap Map.! b)

