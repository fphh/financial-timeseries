

module FinancialTimeseries.Source.Alphavantage where

import Data.Ord (comparing)

import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)

import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Vector.Algorithms.Intro (sortBy)

import qualified Data.Text as Text
import Data.Text (Text)

import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as HM

import qualified Data.Aeson as Ae
import Data.Aeson (Value)

import qualified Network.HTTP.Simple as Simple

import FinancialTimeseries.Source.Row (Row(..), Extract(..), Volume(..))
import FinancialTimeseries.Type.Timeseries (TimeseriesRaw(..))
import FinancialTimeseries.Type.Types (Price(..))


url :: String -> String -> Simple.Request
url sym apikey = Simple.parseRequest_ $
  "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=" ++ sym
  ++ "&outputsize=full&datatype=json&apikey=" ++ apikey

data DataSet a = DataSet {
  information :: Text
  , symbol :: Text
  , lastRefreshed :: UTCTime
  , outputSize :: Text
  , timeZone :: Text
  , timeseriesDS :: a
  } deriving (Show)

toDataSet ::
  (Read a) =>
  Extract a -> Value -> Maybe (DataSet (Vector (UTCTime, Price a)))
toDataSet (Extract extract) (Ae.Object val) =
  let fromString (Ae.String txt) = Just txt
      fromString _ = Nothing
      
      fromObject (Ae.Object obj) = Just obj
      fromObject _ = Nothing
      
      readString key w = HM.lookup (Text.pack key) w >>= fromString
      readObject key w = HM.lookup (Text.pack key) w >>= fromObject
      readNumber key w = HM.lookup (Text.pack key) w >>= fromString >>= readMaybe . Text.unpack
      readTime key w = readString key w >>=
        \x -> case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (Text.unpack x) of
                Nothing -> parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack x)
                t -> t
                
      toVector ts =
        let f k v acc = do
              a <- acc
              w <- fromObject v
              t <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack k)
              o <- readNumber "1. open" w
              h <- readNumber "2. high" w
              l <- readNumber "3. low" w
              c <- readNumber "4. close" w
              vol <- readNumber "5. volume" w
              let !r = extract $ Row {
                    open = Price o
                    , high = Price h
                    , low = Price l
                    , close = Price c
                    , volume = Volume vol
                    }
              return ((t, r) : a)
              
        in fmap (Vec.modify (sortBy (comparing fst)) . Vec.fromList) (HM.foldrWithKey f (Just []) ts)

  in do
    md <- readObject "Meta Data" val
    info <- readString "1. Information" md
    sym <- readString "2. Symbol" md
    lr <- readTime "3. Last Refreshed" md
    os <- readString "4. Output Size" md
    tz <- readString "5. Time Zone" md

    ts <- readObject "Time Series (Daily)" val >>= toVector

    return DataSet {
      information = info
      , symbol = sym 
      , outputSize = os
      , lastRefreshed = lr
      , timeZone = tz
      , timeseriesDS = ts
      }
toDataSet _ _ = Nothing


dataSet2timeseries :: DataSet (Vector (UTCTime, Price a)) -> TimeseriesRaw a
dataSet2timeseries ds = TimeseriesRaw {
  name = Text.unpack (symbol ds)
  , timeseries = Price (Vec.map (fmap unPrice) (timeseriesDS ds))
  }


getSymbol ::
  (Read a) =>
  String -> String -> IO (Maybe (TimeseriesRaw a))
getSymbol sym apikey = do
  response <- Simple.httpJSON (url sym apikey)
  let ds = toDataSet (Extract close) (Simple.getResponseBody response)
  return (fmap dataSet2timeseries ds)
