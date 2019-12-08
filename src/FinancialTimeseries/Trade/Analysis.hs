{-# LANGUAGE FlexibleContexts #-}


module FinancialTimeseries.Trade.Analysis where

import Data.Time (getCurrentTime)

import Data.Distributive (Distributive)

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty

import qualified Graphics.Rendering.Chart.Easy as E

import FinancialTimeseries.Algorithm.Evaluate (Profit)
import qualified FinancialTimeseries.Algorithm.MonteCarlo as AMC

import qualified FinancialTimeseries.Optimize.Optimize as Optimize

import qualified FinancialTimeseries.Render.Document as Document
import qualified FinancialTimeseries.Render.HtmlReader as HtmlReader
import FinancialTimeseries.Render.Render (display)

import qualified FinancialTimeseries.Report.Standard as Standard

import qualified FinancialTimeseries.Source.Binance.Type.BarLength as BarLength
import qualified FinancialTimeseries.Source.Binance.Type.Symbol as Symbol

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Table (Table(..), Cell(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), Length)
import FinancialTimeseries.Type.Types (StripPrice, TimeseriesYield(..), ExchangeRate, Equity(..))

import FinancialTimeseries.Util.Pretty (Pretty)
import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)


data Config params price a = Config {
  outputDirectory :: FilePath
  , barLength :: BarLength.BarLength
  , fractions :: [Fraction a]
  , strategy :: Strategy params price a
  }


analyze ::
  (Length (TimeseriesRaw price a), Profit price, Distributive price, StripPrice price, Show a, Pretty a, Num a, Fractional a, Real a, E.PlotValue a, Pretty params, ToFileString params) =>
  Config params price a -> Symbol.Symbol -> TimeseriesRaw price a -> IO ()
analyze cfg sym s =  do

  now <- getCurrentTime
  repCfg <- HtmlReader.defaultConfig
  mcCfg <- AMC.newMCConfig 100 100 (Equity 1000) (fractions cfg)

  let config = Standard.Config {
        Standard.now = now
        , Standard.reportConfig = repCfg
        , Standard.monteCarloConfig = mcCfg
        , Standard.strategy = strategy cfg
        }

      html = Standard.report config s

      str = Pretty.renderHtml (Document.render html)

      fileName = 
        outputDirectory cfg
        ++ "/" ++ show sym
        ++ "-" ++ toFileString (barLength cfg)
        ++ "-" ++ toFileString (strategy cfg)
        ++ "-historic.html"

  writeFile fileName str


optimize ::
  (StripPrice price, Distributive price, Profit price, Pretty a, Real a, Floating a, Show a, Show params) =>
  Optimize.Config params price a
  -> Symbol.Symbol
  -> TimeseriesRaw price a
  -> IO (params, Optimize.Metrics a)
optimize cfg sym s = do
  
  repCfg <- HtmlReader.defaultConfig

  let opts = Optimize.optimize cfg s

      table = Table
        "Ranked Optimization Parameters"
        [CString "Metrics"]
        (map (\(w, Optimize.Metrics y) -> Labeled (show w) [Cell y]) opts)

      best = last opts
      --t :: H5.Html
      t = HtmlReader.runHtmlReader repCfg (display [table])

  writeFile (Optimize.outputDirectory cfg ++ "/" ++ show sym ++ "-optimize.html") (Pretty.renderHtml (Document.render t))
  return best

