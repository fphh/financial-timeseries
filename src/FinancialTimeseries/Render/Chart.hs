{-# LANGUAGE ScopedTypeVariables #-}

module FinancialTimeseries.Render.Chart where

import Control.Monad (join)

import Control.Monad.Trans.Reader (ReaderT(..), ask)

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
import Text.Blaze.Internal (MarkupM)

import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Renderable as R
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Diagrams.Prelude as DP
import qualified Diagrams.Backend.SVG as DSVG
import qualified Diagrams.TwoD as D2

import Graphics.Svg.Core (renderBS)


import FinancialTimeseries.Render.Css ((!))
import FinancialTimeseries.Type.Evaluate (Long(..))
import FinancialTimeseries.Type.Segment (Segment(..))
import FinancialTimeseries.Type.Timeseries (Timeseries(..))
import FinancialTimeseries.Type.Type.Equity (Equity(..))


data Config = Config {
  chartSize :: (Double, Double)
  , denv :: D.DEnv Double
  }

mkConfig :: Double -> Double -> IO Config
mkConfig w h = do
  fs <- D.loadSansSerifFonts
  let cs = (w, h)
      env = D.createEnv E.vectorAlignmentFns w h fs
  return $ Config {
    chartSize = cs
    , denv = env
    }

defConfig :: IO Config
defConfig = mkConfig 1600 400



type HtmlReader a = ReaderT Config MarkupM a


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
  let f (Timeseries nam ts segs as) =
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
           ++ map (fmap ((:[]) . Vec.toList)) as
                  
  in renderChart (concatMap f vs ++ [("$$$", [Vec.toList (unEquity res)])])


render2 ::
  (E.PlotValue a, Fractional a) =>
  Equity (Vector (UTCTime, a)) -> [Timeseries a] -> Config -> Html
render2 res vs config = join (runReaderT (renderHelper res vs) config)


class Render longOrShort where
  render ::
    (E.PlotValue a, Fractional a) =>
    longOrShort (Equity (Vector (UTCTime, a))) -> [Timeseries a] -> Config -> Html

instance Render Long where
  render (Long res) vs config = do
    H5.h2 $ H5.span $ H5.toHtml (Text.pack "Long")
    render2 res vs config


renderDocument :: Html -> Html
renderDocument chart = do
  H5.docType
  H5.html $ do
    H5.body ! "monoFont" $ do
      H5.h1 $ H5.span $ H5.toHtml (Text.pack "Analysis")
      chart



