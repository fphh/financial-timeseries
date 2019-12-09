{-# LANGUAGE FlexibleContexts #-}


module FinancialTimeseries.Trade.Analysis where

import Control.Monad.IO.Class (liftIO)

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

import FinancialTimeseries.Trade.TradeReaderIO (TradeReaderIO, runTradeReaderIO, outputDirectory)

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Table (Table(..), Cell(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), Length)
import FinancialTimeseries.Type.Types (StripPrice, TimeseriesYield(..), ExchangeRate, Equity(..))

import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)


data Config params price a = Config {
  barLength :: BarLength.BarLength
  , fractions :: [Fraction a]
  , strategy :: Strategy params price a
  }


analyze ::
  (Length (TimeseriesRaw price a), Profit price, Distributive price, StripPrice price, Show a, Pretty a, Num a, Fractional a, Real a, E.PlotValue a, Pretty params, ToFileString params) =>
  Config params price a -> Symbol.Symbol -> TimeseriesRaw price a -> TradeReaderIO ()
analyze cfg sym s =  do
  outDir <- outputDirectory

  liftIO $ do
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
          outDir
          ++ "/" ++ show sym
          ++ "-" ++ toFileString (barLength cfg)
          ++ "-" ++ toFileString (strategy cfg)
          ++ "-historic.html"

    writeFile fileName str


optimize ::
  (StripPrice price, Distributive price, Profit price, Pretty a, Real a, Floating a, Show a, Pretty params) =>
  Optimize.Config params price a
  -> Symbol.Symbol
  -> TimeseriesRaw price a
  -> TradeReaderIO (params, Optimize.Metrics a)
optimize cfg sym s = do
  outDir <- outputDirectory

  liftIO $ do
    repCfg <- HtmlReader.defaultConfig

    let opts = Optimize.optimize cfg s

        table = Table
          "Ranked Optimization Parameters"
          [CString "Metrics"]
          (map (\(w, Optimize.Metrics _ y) -> Labeled (pretty w) [Cell y]) opts)

        best = last opts

        t = HtmlReader.runHtmlReader repCfg (display [table])

        metrics = Optimize.name (snd best)

    writeFile (outDir ++ "/" ++ show sym ++ "-optimized-by-" ++ metrics ++ ".html") (Pretty.renderHtml (Document.render t))
    return best

