{-# LANGUAGE ScopedTypeVariables #-}

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


import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Render.HtmlReader (HtmlReader, Config(..), runHtmlReader)
import FinancialTimeseries.Type.Chart (Chart(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Segment (Segment(..))
import FinancialTimeseries.Type.Timeseries (Timeseries(..))
import FinancialTimeseries.Type.Types (Equity(..), Price(..))
import FinancialTimeseries.Util.Pretty (Pretty, pretty)



renderChart :: (E.PlotValue a) => [(String, [[(UTCTime, a)]])] -> HtmlReader Html
renderChart vs = do
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
    $ R.toRenderable $ do
    mapM_ (\(title, xs) -> E.plot (E.line title xs)) vs

renderHelper ::
  (E.PlotValue a, Fractional a) =>
  Equity (Vector (UTCTime, a)) -> [Timeseries a] -> HtmlReader Html
renderHelper res vs =
  let f (Timeseries nam (Price ts) segs as) =
        let (_, mi) = Vec.minimumBy (compare `on` snd) ts
            (_, ma) = Vec.maximumBy (compare `on` snd) ts
            m = mi + (ma - mi) * 0.8
            spikeLow = m - (ma - mi) * 0.1
            spikeHigh = m + (ma - mi) * 0.1
            g (Segment a b) =
              let (t0, _) = ts Vec.! a
                  (tn, _) = ts Vec.! b
              in [(t0,spikeLow), (t0, spikeHigh), (t0, m), (tn, m), (tn, spikeLow), (tn, spikeHigh)]
        in [(nam, [Vec.toList ts]), (nam ++ " (inv. / not inv.)", map g segs)]
           ++ map (fmap ((:[]) . Vec.toList . unPrice)) as
                  
  in renderChart (concatMap f vs ++ [("Equity", [Vec.toList (unEquity res)])])


render2 ::
  (E.PlotValue a, Fractional a) =>
  Config -> Equity (Vector (UTCTime, a)) -> [Timeseries a] -> Html
render2 config res vs = runHtmlReader config (renderHelper res vs)


chart ::
  (E.PlotValue a, Fractional a) =>
  Equity (Vector (UTCTime, a)) -> [Timeseries a] -> Config -> Html
chart es vs config = render2 config es vs


pdfChart :: (E.PlotValue a, Pretty params) => Chart params a -> HtmlReader Html
pdfChart (Chart ttl cs) = do
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
    $ mapM_ (\(Labeled title xs) -> E.plot (E.line (pretty title) [Vec.toList xs])) cs
