{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FinancialTimeseries.Report.Standard where

import Data.Time (UTCTime)

import Data.Bifunctor (bimap)
import Data.Distributive (Distributive, distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified System.Random as R

import qualified Text.Blaze.Html5 as H5

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Statistics.Sample.Histogram as Histo

import qualified FinancialTimeseries.Algorithm.MonteCarlo as AMC
import FinancialTimeseries.Algorithm.Evaluate (Profit, long, evaluate, evaluateFraction)
import FinancialTimeseries.Render.Chart (chart)
import qualified FinancialTimeseries.Render.HtmlReader as HtmlReader -- (Config, runHtmlReader)
import FinancialTimeseries.Render.Render (display)
import FinancialTimeseries.Render.Statement (statement, currentTime)
import FinancialTimeseries.Statistics.Statistics (yield, tradeStatistics, stats2cdfChart, stats2list)
import FinancialTimeseries.Test (check_timeseries_prop)
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Histogram (Histogram(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.MonteCarlo (Broom(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, first, slice)
import qualified FinancialTimeseries.Type.Timeseries as TS
import FinancialTimeseries.Type.Types (StripPrice, stripPrice, Equity(..), partitionInvested)

import FinancialTimeseries.Util.DistributivePair (distributePair)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)



data Config params gen price a = Config {
  now :: UTCTime
  , reportConfig :: HtmlReader.Config
  , monteCarloConfig :: AMC.Config gen a
  , strategy :: Strategy params price a
  }

report ::
  (Profit price, Distributive price, StripPrice price, Pretty params, R.RandomGen gen, Num a, Fractional a, Real a, E.PlotValue a, Show a, Pretty a, TS.Length (TimeseriesRaw price a)) =>
  Config params gen price a -> TimeseriesRaw price a -> H5.Html
report cfg ts =
  let t = (freeStrategy (strategy cfg)) (parameters (strategy cfg)) ts
      lg = long (partitionInvested (slice t))

      mteCrlo = AMC.mc (monteCarloConfig cfg) lg

      yields =
        let k = fmap (Vec.fromList . map (yield . Vec.map snd))
        in fmap (fmap (bimap k k)) lg

      tradeYields =
        let  j zs = (fmap stats2cdfChart zs, fmap stats2list zs)
             k = j . fmap ((:[]) . Labeled "Trade Yields" . tradeStatistics)
        in fmap (fmap (snd . fmap k)) yields

      tsCharts =
        let convert = Equity . snd . stripPrice
            k = fmap (flip chart [t]) . distribute
        in fmap (bimap k k . distributePair) (evaluate (convert (first ts)) lg)

      histogram =
        let histo zs =
              let n = round (fromIntegral (Vec.length zs) / fromIntegral (5 :: Integer) :: Double)
              in Histogram (Histo.histogram n (Vec.map realToFrac zs) :: (Vector Double, Vector Int))
        in fmap (fmap (fmap histo . snd)) yields

      ms = map (\f -> mteCrlo f (evaluateFraction f)) (AMC.fractions (monteCarloConfig cfg))
      ys = fmap (fmap (fmap snd)) (AMC.timeseriesYields ms)
      rdds = fmap (fmap (fmap snd)) (AMC.relativeDrawdowns ms)

      broom = fmap (fmap (fmap (bimap (fmap Broom) (fmap Broom)))) (mteCrlo (Fraction 1.0) evaluate)
      
      html = HtmlReader.runHtmlReader (reportConfig cfg) $ mconcat $
        currentTime (now cfg)
        : statement ("Timeseries is well formed: " ++ show (check_timeseries_prop t))
        : statement ("Parameters: " ++ pretty (parameters (strategy cfg)))
        : display tsCharts
        : display tradeYields
        : display histogram
        : display broom
        : display ys
        : display rdds
        : []
        
  in html
