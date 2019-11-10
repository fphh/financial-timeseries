{-# LANGUAGE FlexibleContexts #-}

module FinancialTimeseries.Report.Standard where

import Data.Time (UTCTime)

import Data.Bifunctor (bimap)
import Data.Distributive (distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified System.Random as R

import qualified Text.Blaze.Html5 as H5

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Statistics.Sample.Histogram as Histo

import qualified FinancialTimeseries.Algorithm.MonteCarlo as AMC
import FinancialTimeseries.Algorithm.Evaluate (profit, long, short, evaluate, evaluateFraction, evaluateInvested, longEvaluate)
import FinancialTimeseries.Render.Chart (chart)
import FinancialTimeseries.Render.HtmlReader (Config, runHtmlReader)
import FinancialTimeseries.Render.Render (display)
import FinancialTimeseries.Render.Statement (statement, currentTime)
import FinancialTimeseries.Statistics.Statistics (mkStatistics, yield)
import FinancialTimeseries.Test (check_timeseries_prop)
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Histogram (Histogram(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.MonteCarlo (Broom(..))
import FinancialTimeseries.Type.Strategy (Strategy(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw, first, slice)
import FinancialTimeseries.Type.Types (Equity(..), Price(..), partitionInvested)

import FinancialTimeseries.Util.DistributivePair (distributePair)
import FinancialTimeseries.Util.Pretty (Pretty)


data ReportConfig gen a = ReportConfig {
  now :: UTCTime
  , reportConfig :: Config
  , monteCarloConfig :: AMC.Config gen a
  , strategy :: Strategy a -- TimeseriesRaw a -> Timeseries a
  }

report ::
  (R.RandomGen gen, Num a, Fractional a, Real a, E.PlotValue a, Show a, Pretty a) =>
  ReportConfig gen a -> TimeseriesRaw a -> H5.Html
report cfg ts =
  let t = unStrategy (strategy cfg) ts
      lg = long (partitionInvested (slice t))
      mteCrlo = AMC.mc (monteCarloConfig cfg) lg

      yields =
        let k = fmap (Vec.fromList . map (yield . Vec.map snd))
        in fmap (fmap (bimap k k)) lg

      tradeYields =
        let  j ys = (fmap AMC.stats2cdfChart ys, fmap AMC.stats2list ys)
             k = j . fmap ((:[]) . Labeled "Trade Yields" . mkStatistics)
        in fmap (fmap (snd . fmap k)) yields
      

      tsCharts =
        let convert (Price x) = Equity (snd x)
            k = fmap (flip chart [t]) . distribute
        in fmap (bimap k k . distributePair) (evaluate (convert (first ts)) lg)

      histogram =
        let histo ys =
              let n = round (fromIntegral (Vec.length ys) / fromIntegral 5)
              in Histogram (Histo.histogram n (Vec.map realToFrac ys) :: (Vector Double, Vector Int))
        in fmap (fmap (fmap histo . snd)) yields

      ms = map (\f -> mteCrlo f (evaluateFraction f)) (AMC.fractions (monteCarloConfig cfg))
      ys = fmap (fmap (fmap snd)) (AMC.timeseriesYields ms)
      rdds = fmap (fmap (fmap snd)) (AMC.relativeDrawdowns ms)

      broom = fmap (fmap (fmap (bimap (fmap Broom) (fmap Broom)))) (mteCrlo (Fraction 1.0) evaluate)

      
      html = runHtmlReader (reportConfig cfg) $ mconcat $
        currentTime (now cfg)
        : statement ("Timeseries is well formed: " ++ show (check_timeseries_prop t))
        : display tsCharts
        : display tradeYields
        : display histogram
        : display broom
        : display ys
        : display rdds
        : []
        
  in html
