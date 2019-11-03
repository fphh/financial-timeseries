{-# LANGUAGE FlexibleInstances #-}


module FinancialTimeseries.Render.Render where

import Control.Applicative (liftA2)
-- import Control.Monad (liftM)

import qualified Data.List as List

import Data.Vector (Vector)

import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import qualified Graphics.Rendering.Chart.Easy as E


import FinancialTimeseries.Render.Chart (pdfChart)
import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Render.HtmlReader (HtmlReader, Config, runHtmlReader)
import FinancialTimeseries.Render.MonteCarlo (renderMC)
import FinancialTimeseries.Render.Table (table)
import FinancialTimeseries.Type.Chart (Chart(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Table (Table)
import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..), Equity(..), Yield(..), Price(..), AbsoluteDrawdown(..), RelativeDrawdown(..))
import FinancialTimeseries.Type.Short (Short(..))
import FinancialTimeseries.Util.Pretty (Pretty, pretty)




class Render a where
  render :: [String] -> a -> HtmlReader Html

display :: (Render a) => a -> HtmlReader Html
display = render []

instance (Render a) => Render (MonteCarlo a) where
  render xs (MonteCarlo m) = render (xs ++ ["MonteCarlo"]) m

instance (Render a) => Render (Invested a) where
  render xs (Invested m) = render (xs ++ ["Invested"]) m

instance (Render a) => Render (NotInvested a) where
  render xs (NotInvested m) = render (xs ++ ["NotInvested"]) m

instance (Render a) => Render (Equity a) where
  render xs (Equity m) = render (xs ++ ["Equity"]) m

instance (Render a) => Render (Yield a) where
  render xs (Yield m) = render (xs ++ ["Yield"]) m

instance (Render a) => Render (Price a) where
  render xs (Price m) = render (xs ++ ["Price"]) m

instance (Render a) => Render (AbsoluteDrawdown a) where
  render xs (AbsoluteDrawdown m) = render (xs ++ ["AbsoluteDrawdown"]) m

instance (Render a) => Render (RelativeDrawdown a) where
  render xs (RelativeDrawdown m) = render (xs ++ ["RelativeDrawdown"]) m

-- instance (Render a) => Render [a] where
--  render xs ys = sequence (map (render xs) ys)

instance (Render a, Render b) => Render (a, b) where
  render xs (u, v) = liftA2 (<>) (render xs u) (render xs v)

instance (Render a) => Render (Long a) where
  render xs (Long m) = render (xs ++ ["Long"]) m

instance (Render a) => Render (Short a) where
  render xs (Short m) = render (xs ++ ["Short"]) m

instance (E.PlotValue a) => Render (Vector (Vector a)) where
  render xs vs =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in fmap (h <>) (renderMC vs)

instance (Pretty params, Render a) => Render (Labeled params a) where
  render xs (Labeled lbl ys) = render (xs ++ [pretty lbl]) ys

instance (Real a, Pretty params) => Render [Table params a] where
  render xs ts =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in return (h <> (H5.div ! "tables" $ mapM_ table ts))


instance (Real a, E.PlotValue a, Pretty params) => Render (Chart params a) where
  render xs c =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in fmap (h <>) (pdfChart c)

