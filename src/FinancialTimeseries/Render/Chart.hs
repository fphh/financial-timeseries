{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}



module FinancialTimeseries.Render.Chart where

import Control.Monad.Trans.Reader (ask)

import Data.Function (on)

import Data.Time (UTCTime)


import qualified Data.Text as Text

import qualified Data.Vector as Vec
import Data.Vector (Vector)


import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Search as BSS

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Renderable as R
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Diagrams.Prelude as DP
import qualified Diagrams.Backend.SVG as DSVG
import qualified Diagrams.TwoD as D2

import Graphics.Svg.Core (renderBS)

import FinancialTimeseries.Render.HtmlReader (HtmlReader, Config(..))
import FinancialTimeseries.Type.Chart (ParaCurve(..), Chart(..), LChart, add)
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), Timeseries(..))
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, Equity(..))
import FinancialTimeseries.Util.Pretty (Pretty, pretty)



chart ::
  (StripPrice price, Real a) =>
  [Timeseries price a] -> LChart String UTCTime Double
chart vs =
  let f (Timeseries (TimeseriesRaw nam tsVec) segs halfSeg as) =
        let ts = Vec.map (fmap realToFrac) (stripPrice tsVec)
            (_, mi) = Vec.minimumBy (compare `on` snd) ts
            (_, ma) = Vec.maximumBy (compare `on` snd) ts
            m = mi + (ma - mi) * 0.8
            spikeLow = m - (ma - mi) * 0.1
            spikeHigh = m + (ma - mi) * 0.1
            g (Segment a b) =
              let (t0, _) = ts Vec.! a
                  (tn, _) = ts Vec.! b
                  ys = [(t0,spikeLow), (t0, spikeHigh), (t0, m), (tn, m), (tn, spikeLow), (tn, spikeHigh)]
              in Vec.fromList ys
            h (HalfSegment a) =
              let (t0, _) = ts Vec.! a
                  (tn, _) = Vec.last ts
                  ys = [(t0,spikeLow), (t0, spikeHigh), (t0, m), (tn, m)]
              in Vec.fromList ys
            hs = maybe [] ((:[]) . h) halfSeg
            zs = map (Vec.map (fmap realToFrac)) (map g segs ++ hs)
        in [Labeled nam [ts], Labeled (nam ++ " (inv. / not inv.)") zs]
           ++ map (fmap (:[])) as
  in Chart "Timeseries" (concatMap f vs)


chartWithEquity ::
  (StripPrice price, Real a) =>
  Equity (Vector (UTCTime, a)) -> [Timeseries price a] -> LChart String UTCTime Double
chartWithEquity eqty vs =
  let leqty = Labeled "Equity" [Vec.map (fmap realToFrac) (unEquity eqty)]
  in add leqty (chart vs)


class PlotLine a where
  type TyX a :: *
  type TyY a :: *
  plotLine :: (Pretty params) => Labeled params a -> E.EC (E.Layout (TyX a) (TyY a)) ()

instance PlotLine [Vector (x, a)] where
  type TyX [Vector (x, a)] = x
  type TyY [Vector (x, a)] = a
  plotLine (Labeled ttle xs) = E.plot (E.line (pretty ttle) (map Vec.toList xs))


instance PlotLine (ParaCurve Vector x a) where
  type TyX (ParaCurve Vector x a) = x
  type TyY (ParaCurve Vector x a) = a
  plotLine (Labeled ttle (ParaCurve curveparams xs)) = do
    curveparams
    E.plot (E.line (pretty ttle) (map Vec.toList xs))




renderChart ::
  (PlotLine curve, E.PlotValue (TyX curve), E.PlotValue (TyY curve), Pretty params) =>
  Chart params curve -> HtmlReader Html
renderChart (Chart ctitle cs) = do
  cfg <- ask
  
  let (w, h) = chartSize cfg
      env = denv cfg
      rr = BS.pack [99,108,105,112,45,112,97,116,104,61]
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing Text.empty [] True

  return
    $ H5.preEscapedToHtml
    $ BSL.unpack
    $ BSS.replace rr BSL.empty 
    $ renderBS
    $ DP.renderDia DSVG.SVG opts
    $ (\(d, _, _) -> d)
    $ D.runBackendWithGlyphs env
    $ flip R.render (w, h)
    $ R.toRenderable
    $ do
    E.layout_title DP..= ctitle
    mapM_ plotLine cs
