

module FinancialTimeseries.Render.HtmlReader where

import Control.Monad (join)

import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Graphics.Rendering.Chart.Easy as E

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)



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
defConfig = mkConfig 1200 1200



type HtmlReader a = ReaderT Config MarkupM a


runHtmlReader ::
  HtmlReader Html -> Config -> Html
runHtmlReader htmlReader = join . runReaderT htmlReader

