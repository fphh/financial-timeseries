

module FinancialTimeseries.Source.Binance.Collector where

import qualified System.IO as Sys

import Control.Monad (void)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)


import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (ExchangeRate(..))

import qualified FinancialTimeseries.Source.Binance.OrderBook as OrderBook

import FinancialTimeseries.Source.Binance.Type.Ask (Ask(..))
import qualified FinancialTimeseries.Source.Binance.Type.BarLength as BarLength
import FinancialTimeseries.Source.Binance.Type.BarLength (BarLength)
import FinancialTimeseries.Source.Binance.Type.Bid (Bid(..))
import FinancialTimeseries.Type.ByQuantity (ByQuantity(..), ExchangeRateByQuantity(..))
import FinancialTimeseries.Source.Binance.Type.Symbol (Symbol)


data Config = Config {
  barLength :: BarLength
  , outputDir :: FilePath
  , depth :: OrderBook.Limit
  }


defaultConfig :: Config
defaultConfig = Config {
  barLength = BarLength.Min 1
  , outputDir = "dta/"
  , depth = OrderBook.Depth100
  }



data CollectedData = CollectedData {
  requestStart :: UTCTime
  , requestEnd :: UTCTime
  , orderBook :: OrderBook.Response Double
  } deriving (Show, Read, Eq)


serialize :: CollectedData -> String
serialize (CollectedData start end ob) =
  let bs = unzip (map (\(ByQuantity (Bid p) q) -> (p, q)) (OrderBook.bids ob))
      as = unzip (map (\(ByQuantity (Ask p) q) -> (p, q)) (OrderBook.asks ob))
  in show (start, end, OrderBook.lastUpdateId ob, bs, as)
  
unserialize :: String -> [CollectedData]
unserialize xs =
  let ws = lines xs
      zs = map read ws
      byQty g p q = ByQuantity (g p) q
      f (start, end, luID, (bp, bq), (ap, aq)) =
        CollectedData start end (OrderBook.Response luID (zipWith (byQty Bid) bp bq) (zipWith (byQty Ask) ap aq))
  in map f zs

collector :: MVar a -> Config -> Symbol -> IO ()
collector mvar cfg sym = do  
  let fileName =
        outputDir cfg
        ++ show sym
        ++ "-" ++ show (barLength cfg)
        ++ ".dta"

  hd <- Sys.openFile fileName Sys.AppendMode
  Sys.hSetBuffering hd Sys.LineBuffering

  let loop = do
        void (takeMVar mvar)

        start <- getCurrentTime
        ob <- OrderBook.get (OrderBook.defaultQuery sym (depth cfg))
        end <- getCurrentTime
        
        Sys.hPutStrLn hd (serialize (CollectedData start end ob))
        
        loop
        
  loop

scheduler :: Config -> [MVar ()] -> IO ()
scheduler cfg mvars = do
  ts <- BarLength.nextTimeSlices (barLength cfg)

  let f t = do
        now <- getCurrentTime
        let delta = t `diffUTCTime` now
        threadDelay (round (realToFrac delta * 1000 * 1000 :: Double))
        mapM_ (flip putMVar ()) mvars
        putStrLn ("Scheduled new request at " ++ show t)
        
  mapM_ f ts

collect :: Config -> [Symbol] -> IO ()
collect cfg syms = do
  
  let g sym = do
        mvar <- newEmptyMVar
        void (forkIO (collector mvar cfg sym))
        return mvar

  mvars <- mapM g syms
  
  scheduler cfg mvars

  let loop = do
        threadDelay (60*1000*1000)
        loop

  loop


askByQuantity ::
  (Ord a, Num a, Fractional a) =>
  Double -> [ByQuantity (Ask a)] -> ByQuantity (Ask a)
askByQuantity qty as =
  let go _ [] = []
      go n (ByQuantity (Ask p) q : xs) =
        case n <= 0 of
          True -> []
          False -> p * realToFrac (min q n) : go (n-q) xs

      qs = go qty as

  in ByQuantity (Ask (sum qs / realToFrac qty)) qty
    

bidByQuantity ::
  (Ord a, Num a, Fractional a) =>
  Double -> [ByQuantity (Bid a)] -> ByQuantity (Bid a)
bidByQuantity qty as =
  let go _ [] = []
      go n (ByQuantity (Bid p) q : xs) =
        case n <= 0 of
          True -> []
          False -> p * realToFrac (min q n) : go (n-q) xs

      qs = go qty as

  in ByQuantity (Bid (sum qs / realToFrac qty)) qty


exchangeRateByQuantity ::
  String -> Double -> Vector CollectedData -> TimeseriesRaw ExchangeRateByQuantity (Bid Double, Ask Double)
exchangeRateByQuantity nam qty vs =
  let f (CollectedData start end (OrderBook.Response _ bs as)) =
        let t = ((end `diffUTCTime` start) / 2) `addUTCTime` start
            ByQuantity us _ = bidByQuantity qty bs
            ByQuantity ws _ = askByQuantity qty as
        in (t, (us, ws))
      g x qty = ExchangeRate (ByQuantity x qty)
  in TimeseriesRaw {
    name = nam
    , timeseries = ExchangeRateByQuantity (ExchangeRate (ByQuantity (Vec.map f vs) qty))
    }

readCollectedData :: FilePath -> IO (Vector CollectedData)
readCollectedData file = readFile file >>= return . Vec.fromList . unserialize




{-
serialize :: CollectedData -> Text
serialize (CollectedData start end ob) =
  let bs = unzip (map (\(Bid p q) -> (p, q)) (OrderBook.bids ob))
      as = unzip (map (\(Ask p q) -> (p, q)) (OrderBook.asks ob))
  in Enc.decodeUtf8 (GZip.compress (Enc.encodeUtf16BE (Text.pack (show (start, end, OrderBook.lastUpdateId ob, bs, as)))))



unserialize :: Text -> [CollectedData]
unserialize xs =
  let ws = Text.lines xs
      zs = map (read . Text.unpack . Enc.decodeUtf16BE . GZip.decompress . Enc.encodeUtf8) ws
      f (start, end, luID, (bp, bq), (ap, aq)) =
        CollectedData start end (OrderBook.Response luID (zipWith Bid bp bq) (zipWith Ask ap aq))
  in map f zs
-}
