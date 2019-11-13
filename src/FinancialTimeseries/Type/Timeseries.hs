{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FinancialTimeseries.Type.Timeseries where


import Data.Time (UTCTime, addUTCTime, parseTimeM, defaultTimeLocale)

import Data.Bifunctor (bimap)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Text.Printf (PrintfArg, printf)

import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.Segment (Segment(..), HalfSegment(..), segments)
import FinancialTimeseries.Type.Types (NotInvested, Invested, Price(..))
import FinancialTimeseries.Util.Pretty (Pretty, pretty)


data TimeseriesRaw a = TimeseriesRaw {
  name :: String
  , timeseries :: Price (Vector (UTCTime, a))
  } deriving (Show, Read)

data Timeseries a = Timeseries {
  timeseriesRaw :: TimeseriesRaw a
  , investedSegments :: [Segment]
  , lastSegment :: Maybe HalfSegment
  , additionalSeries :: [Labeled String (Price (Vector (UTCTime, a)))]
  } deriving (Show, Read)


first :: TimeseriesRaw a -> Price (UTCTime, a)
first = fmap Vec.head . timeseries


timeline :: forall a. (Ord a, PrintfArg a) => [Segment] -> Maybe HalfSegment -> Vector (UTCTime, a) -> [Vector (UTCTime, a)] -> [String]
timeline is hs v vs =
  let idx i = if or (map (\(Segment a b) -> a <= i && i <= b) is) then 'i' else ' '

      m = Map.fromList (Vec.toList (Vec.imap (\i (t, x) -> (t, Right (idx i, i, x))) v))
      ms = map (Map.fromList . Vec.toList . Vec.map (fmap Left)) vs
      ss = map (Set.fromList . Map.keys) (m:ms)
      tl = List.sort (Set.toList (Set.unions ss))
      g :: Either a (Char, Int, a) -> String
      g (Left x) = printf "%8.4f" x
      g (Right (c, i, x)) = printf "%2c%4d%8.4f" c i x
      f k = show k ++ " | " ++ concatMap (maybe (replicate 24 ' ') g . Map.lookup k) (m:ms)
      res = map f tl
  in res ++ ["\n" ++ show hs]

instance (Show a, Ord a, PrintfArg a) => Pretty (Timeseries a) where
  pretty (Timeseries (TimeseriesRaw n as) ss hs vs) =
    let tl = timeline ss hs (unPrice as) (map (unPrice . content) vs)
    in n ++ "\n" ++ (map (const '-') n) ++ "\n" ++ List.intercalate "\n" tl

-- Only full segments. Ignoring last half segment.
slice :: Timeseries a -> Price [(Either (NotInvested (Vector (UTCTime, a))) (Invested (Vector (UTCTime, a))))]
slice (Timeseries (TimeseriesRaw _ (Price as)) is _ _) =
  let ss = segments is
      f (Segment a b) = Vec.slice a (b-a+1) as
  in Price (map (bimap (fmap f) (fmap f)) ss)


timeseriesTest :: Timeseries Double
timeseriesTest =
  let Just d = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe UTCTime
  in Timeseries {
    timeseriesRaw = TimeseriesRaw {
        name = "timeseriesTest"
        , timeseries = Price (Vec.generate 40 (\i -> (realToFrac i `addUTCTime` d, 2 + sin (0.5*fromIntegral i))))
        }
    , investedSegments = [Segment 2 3, Segment 7 9]
    , lastSegment = Nothing
    , additionalSeries = []
    }



    
{-
instance (Show a, Ord a, PrintfArg a) => Pretty (Timeseries a) where
  pretty (Timeseries n as ss bs) =
    let tl = timeline as (map snd bs)
    in List.intercalate "\n" tl
        idx i is = or (map (\(Segment a b) -> a <= i && i <= b) is)
        f i (t, x) acc =
          show i ++ "\t"
          ++ printf "%c" (if idx i ss then 'i' else ' ')
          ++ "\t" ++ show t ++ "\t" ++ show x ++ "\n"
          ++ acc
    in n ++ "\n" ++ (map (const '-') n) ++ "\n" ++ Vec.ifoldr' f "" as
-}
