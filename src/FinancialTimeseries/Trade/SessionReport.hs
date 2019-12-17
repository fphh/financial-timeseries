{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Trade.SessionReport where

import Control.Monad.IO.Class (liftIO)

import Data.Time (getCurrentTime)

import Data.Maybe (catMaybes)

import qualified Data.Vector as Vec

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty

import FinancialTimeseries.Trade.Account (Account(..))
import FinancialTimeseries.Trade.LoggerData (LoggerData(..), parse)

import qualified FinancialTimeseries.Algorithm.MonteCarlo as AMC
import FinancialTimeseries.Algorithm.MovingAverage (Window(..), movingAverageStrategy)

import qualified FinancialTimeseries.Render.Document as Document
import qualified FinancialTimeseries.Render.HtmlReader as HtmlReader

import qualified FinancialTimeseries.Report.Standard as Standard

import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength)

import qualified FinancialTimeseries.Trade.Paper.Paper as Paper
import FinancialTimeseries.Trade.TradeReaderIO (TradeReaderIO, fileNamePrefix)

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..))
import qualified FinancialTimeseries.Type.Timeseries as TS
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..), Timeseries(..))
import FinancialTimeseries.Type.Types (Equity(..), ExchangeRate(..))

import FinancialTimeseries.Util.Pretty (Pretty)
import FinancialTimeseries.Util.ToFileString (ToFileString)

rep ::
  (ToFileString params, Pretty params) =>
  BarLength -> Paper.Config params Double -> TradeReaderIO ()
rep bl cfg = do

  filePrefix <- fileNamePrefix (Paper.symbol cfg) bl (Paper.strategy cfg)

  liftIO $ do

    let csvFile = filePrefix ++ ".csv"
        paramsFile = filePrefix ++ ".params"

        fs = map Fraction [0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0]

  
    loggerData :: [LoggerData ExchangeRate Double] <- parse csvFile
    -- (strgy, bl, params) :: (String, BarLength, params) <- fmap readFile paramsFile 

    now <- getCurrentTime
    repCfg <- HtmlReader.defaultConfig
    mcCfg <- AMC.newMCConfig 100 100 (Equity 1000) fs

    let g i a b =
          case baseCurrency (laccount a) /= baseCurrency (laccount b) of
            True -> Just i
            False -> Nothing

        as = catMaybes (zipWith3 g [1..] loggerData (tail loggerData))

        h [] = (Nothing, [])
        h [i] = (Just (HalfSegment i), [])
        h (a:b:cs) = fmap (Segment a b :) (h cs)
      
        (halfSeg, segs) = h as

        f ld = (ltime ld, unExchangeRate (lcurrencyPair ld))
        tsRaw = TimeseriesRaw {
          TS.name = csvFile
          , TS.timeseries = ExchangeRate (Vec.fromList (map f loggerData))
          }

        n = baseCurrency (laccount (head loggerData)) / unExchangeRate (lcurrencyPair (head loggerData))

        k ld =
          let Account bc qc = laccount ld
              ExchangeRate cp = lcurrencyPair ld
          in (ltime ld, (bc + (1/cp)*qc) / n)

  
        eqty = Vec.fromList (map k loggerData)

        ts = Timeseries {
          timeseriesRaw = tsRaw
          , investedSegments = segs
          , lastSegment = halfSeg
          , additionalSeries = [Labeled "Equity (measured)" eqty]
          }

        config = Standard.Config {
          Standard.now = now
          , Standard.reportConfig = repCfg
          , Standard.monteCarloConfig = mcCfg
          , Standard.strategy = Paper.strategy cfg
          }
        
        html = Standard.report config tsRaw

        str = Pretty.renderHtml (Document.render html)

    writeFile (filePrefix ++ "-mesured.html") str


sessionReports ::
  (ToFileString params, Pretty params) =>
  [(BarLength, [Paper.Config params Double])] -> TradeReaderIO ()
sessionReports = mapM_ (uncurry (mapM . rep))
