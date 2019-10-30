


module FinancialTimeseries.Render.MonteCarlo where

import Control.Monad (join)

import Control.Monad.Trans.Reader (ReaderT(..), ask)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Text as Text

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Search as BSS

import Data.Colour.SRGB (sRGB24)

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html (Html)

import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Renderable as R
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Diagrams.Prelude as DP
import qualified Diagrams.Backend.SVG as DSVG
import qualified Diagrams.TwoD as D2

import Graphics.Svg.Core (renderBS)

import FinancialTimeseries.Render.HtmlReader (HtmlReader, Config(..), runHtmlReader)
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..))



renderMC :: (E.PlotValue a) => Vector (Vector a) ->  HtmlReader Html
renderMC vs = do
  cfg <- ask

  let (w, h) = chartSize cfg
      env = denv cfg
      rr = BS.pack [99,108,105,112,45,112,97,116,104,61]
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing Text.empty [] True

      idx :: [Double]
      idx = [0..]

      f v = do
        let z = zip idx (Vec.toList v)

            color =
              case Vec.head v < Vec.last v of
                True -> E.opaque (sRGB24 0 0 220)
                False -> E.opaque (sRGB24 220 0 0)
                
        E.setColors [color]
        E.plot (E.line "" [z])
  
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
    Vec.mapM_ f vs


renderHelper ::
  (E.PlotValue a) =>
  MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a))) -> HtmlReader Html
renderHelper (MonteCarlo (NotInvested nis, Invested is)) = do
  chartInv <- renderMC is
  chartNotInv <- renderMC nis
  return $ do
    H5.h2 (H5.toHtml (Text.pack "Monte Carlo (Invested)"))
    chartInv
    H5.h2 (H5.toHtml (Text.pack "Monte Carlo (Not Invested)"))
    chartNotInv

  

render ::
  (E.PlotValue a) =>
  Config -> MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a))) -> Html
render config mc = runHtmlReader (renderHelper mc) config



renderPDFHelper ::
  (E.PlotValue a) =>
  [(String, Vector (Double, a))] -> HtmlReader Html
renderPDFHelper pdfs = do
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
    $ mapM_ (E.plot . uncurry E.line . fmap ((:[]) . Vec.toList)) pdfs


renderPDF ::
  (E.PlotValue a) =>
  Config -> [(String, Vector (Double, a))] -> Html
renderPDF config pdfs = runHtmlReader (renderPDFHelper pdfs) config
