

module FinancialTimeseries.Trade.TradeReaderIO where

import qualified System.Directory as Dir

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, iso8601DateFormat)

import Control.Monad.Trans.Reader (ReaderT(..), ask)

import FinancialTimeseries.Util.ToFileString (ToFileString, toFileString)


data Config = Config {
  directory :: String
  , csvPostfix :: String
  , htmlPostfix :: String
  } deriving (Show)

mkConfig :: FilePath -> IO Config
mkConfig dir = do
  now <- getCurrentTime
  let d = dir ++ "/" ++ formatTime defaultTimeLocale (iso8601DateFormat (Just "%X%Z")) now     
  Dir.createDirectoryIfMissing True d
  
  return $ Config {
    directory = d
    , csvPostfix = "csv"
    , htmlPostfix = "html"
    }


defaultConfig :: IO Config
defaultConfig = mkConfig "output"



type TradeReaderIO a = ReaderT Config IO a


runTradeReaderIO :: Config -> TradeReaderIO a -> IO a
runTradeReaderIO cfg = flip runReaderT cfg


fileNamePrefix ::
  (Show symbol, ToFileString barlength, ToFileString strategy) =>
  symbol -> barlength -> strategy -> TradeReaderIO FilePath
fileNamePrefix sym bl strgy = do
  dir <- fmap directory ask
  return $
    dir
    ++ "/" ++ show sym
    ++ "-" ++ toFileString bl
    ++ "-" ++ toFileString strgy
  
outputDirectory :: TradeReaderIO FilePath
outputDirectory = fmap directory ask
