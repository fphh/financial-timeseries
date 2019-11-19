{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module FinancialTimeseries.Render.Render where

import Control.Applicative (liftA2)

import Data.Time (UTCTime)

import Data.Colour.SRGB (sRGB24)

import qualified Data.List as List

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Text as Text

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import qualified Graphics.Rendering.Chart.Easy as E


import FinancialTimeseries.Render.Chart (renderChart)
import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Render.HtmlReader (HtmlReader)
import FinancialTimeseries.Render.Table (table)
import FinancialTimeseries.Type.Chart (Chart(..), LChart, ParaCurve(..))
import FinancialTimeseries.Type.Histogram (Histogram(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.MonteCarlo (Broom(..), MonteCarlo(..))
import FinancialTimeseries.Type.Table (Table)
import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..), Equity(..), TimeseriesYield(..), TradeYield(..), Price(..), AbsoluteDrawdown(..), RelativeDrawdown(..))
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

instance (Render a) => Render (TimeseriesYield a) where
  render xs (TimeseriesYield m) = render (xs ++ ["TimeseriesYield"]) m

instance (Render a) => Render (TradeYield a) where
  render xs (TradeYield m) = render (xs ++ ["TradeYield"]) m

instance (Render a) => Render (Price a) where
  render xs (Price m) = render (xs ++ ["Price"]) m

instance (Render a) => Render (AbsoluteDrawdown a) where
  render xs (AbsoluteDrawdown m) = render (xs ++ ["AbsoluteDrawdown"]) m

instance (Render a) => Render (RelativeDrawdown a) where
  render xs (RelativeDrawdown m) = render (xs ++ ["RelativeDrawdown"]) m

instance (Render a, Render b) => Render (a, b) where
  render xs (u, v) = liftA2 (<>) (render xs u) (render xs v)

instance (Render a) => Render (Long a) where
  render xs (Long m) = render (xs ++ ["Long"]) m

instance (Render a) => Render (Short a) where
  render xs (Short m) = render (xs ++ ["Short"]) m

instance (Ord a, E.PlotValue a) => Render (Broom a) where
  render xs (Broom vs) =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))

        p v = Vec.head v < Vec.last v

        winColor = E.opaque (sRGB24 0 0 220)
        looseColor = E.opaque (sRGB24 220 0 0)

        ww = Vec.findIndex p vs
        ll = Vec.findIndex (not . p) vs

        f i v =
          let ttl = 
                case (fmap (i==) ww, fmap (i==) ll) of
                  (Just True, Just False) -> "Winners"
                  (Just False, Just True) -> "Loosers"
                  _ -> ""
              color =
                case p v of
                  True -> winColor
                  False -> looseColor
          in Labeled ttl (ParaCurve (E.setColors [color]) [Vec.imap (,) v])
                       
        c = Chart "Broom" (Vec.toList (Vec.imap f vs))

    in fmap (h <>) (renderChart c)

instance (Pretty params, Render a) => Render (Labeled params a) where
  render xs (Labeled lbl ys) = render (xs ++ [pretty lbl]) ys

instance (Real a, Pretty a, Pretty params) => Render [Table params a] where
  render xs ts =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in return (h <> (H5.div ! "tables" $ mapM_ table ts))

instance (Real a, E.PlotValue a, Pretty params) => Render (LChart params UTCTime a) where
  render xs c =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in fmap (h <>) (renderChart c)

instance (Real a, E.PlotValue a, Pretty params) => Render (LChart params Double a) where
  render xs c =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
    in fmap (h <>) (renderChart c)

instance Render (Histogram (Vector Double, Vector Int)) where
  render xs (Histogram (bs, hs)) =
    let h = H5.h1 $ H5.span $ H5.toHtml (Text.pack (List.intercalate ", " xs))
        db = (bs Vec.! 1) - (bs Vec.! 0)
        f x y = Vec.fromList [(x, 0), (x, y), (x+db, y), (x+db, 0)]
        us = Vec.concat (Vec.toList (Vec.zipWith f bs hs))

        c = Chart "Trade Yield Histogram" [Labeled "Yield Distribution" [us]]
    in fmap (h <>) (renderChart c)
