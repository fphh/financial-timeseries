{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FinancialTimeseries.Render.HtmlReader where

import Control.Monad (join, liftM2)

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
defConfig = mkConfig 600 600



type HtmlReader a = ReaderT Config MarkupM a

instance (Semigroup a) => Semigroup (HtmlReader a) where
  a <> b = liftM2 (<>) a b

instance (Monoid a) => Monoid (HtmlReader a) where
  mempty = return mempty

runHtmlReader :: Config -> HtmlReader Html -> Html
runHtmlReader cfg = join . flip runReaderT cfg

