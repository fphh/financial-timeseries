
module FinancialTimeseries.Report.Standard where

import Data.Time (UTCTime)

import Data.Bifunctor (bimap)
import Data.Distributive (distribute)

import qualified Data.Vector as Vec

import qualified System.Random as R

import qualified Text.Blaze.Html5 as H5

import qualified Graphics.Rendering.Chart.Easy as E


import qualified FinancialTimeseries.Algorithm.MonteCarlo as AMC
import FinancialTimeseries.Algorithm.Evaluate (profit, long, short, evaluate, evaluateFraction, evaluateInvested, longEvaluate)
import FinancialTimeseries.Render.Chart (chart)
import FinancialTimeseries.Render.HtmlReader (Config, runHtmlReader)
import FinancialTimeseries.Render.Render (display)
import FinancialTimeseries.Render.Statement (statement, currentTime)
import FinancialTimeseries.Statistics.Statistics (mkStatistics, yield)
import FinancialTimeseries.Test (check_timeseries_prop)
import FinancialTimeseries.Type.Types (Equity(..), Price(..), partitionInvested)
import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Timeseries (Timeseries, first, slice)
import FinancialTimeseries.Util.DistributivePair (distributePair)
import FinancialTimeseries.Util.Pretty (Pretty)


data ReportConfig gen a = ReportConfig {
  now :: UTCTime
  , reportConfig :: Config
  , monteCarloConfig :: AMC.Config gen a
  , strategy :: Timeseries a -> Timeseries a
  }


report ::
  (R.RandomGen gen, Num a, Fractional a, Real a, E.PlotValue a, Show a, Pretty a) =>
  ReportConfig gen a -> Timeseries a -> H5.Html
report cfg ts =
  let t = strategy cfg ts
      lg = long (partitionInvested (slice t))

      convert (Price x) = Equity (snd x)
      l = evaluate (convert (first ts)) lg

      j ys = (fmap AMC.stats2cdfChart ys, fmap AMC.stats2list ys)
      k = j . fmap ((:[]) . Labeled "Trade Yields" . mkStatistics . Vec.fromList . map (yield . Vec.map snd))
      -- tradeYields = fmap (fmap (bimap k k)) lg
      tradeYields = fmap (fmap (snd . fmap k)) lg

      tsCharts = fmap (bimap (fmap (flip chart [t]) . distribute) (fmap (flip chart [t]) . distribute) . distributePair) l

      mteCrlo = AMC.mc (monteCarloConfig cfg) lg

      ms = map (\f -> mteCrlo f (evaluateFraction f)) (AMC.fractions (monteCarloConfig cfg))
      ys = fmap (fmap (fmap snd)) (AMC.timeseriesYields ms)
      rdds = fmap (fmap (fmap snd)) (AMC.relativeDrawdowns ms)

      html = runHtmlReader (reportConfig cfg) $ mconcat $
        currentTime (now cfg)
        : statement ("Timeseries is well formed: " ++ show (check_timeseries_prop t))
        : display tsCharts
        : display tradeYields
        : display (mteCrlo (Fraction 1.0) evaluate)
        : display ys
        : display rdds
        : []
        
  in html
