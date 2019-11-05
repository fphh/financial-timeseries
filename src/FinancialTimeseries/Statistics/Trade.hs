

module FinancialTimeseries.Statistics.Trade where

import Data.Bifunctor (bimap)
import Data.Biapplicative (biliftA2)

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Text.Lazy as TL

import qualified Statistics.Sample as Sample

import qualified Formatting as F
import qualified Formatting.Time as FT

import FinancialTimeseries.Type.Types (Yield(..))


newtype ROI a = ROI {
  unROI :: a
  } deriving (Show)

data Stats a = Stats {
  count :: Int
  , longestDuration :: NominalDiffTime
  , shortestDuration :: NominalDiffTime
  , meanDuration :: NominalDiffTime
  , stdDevDuration :: NominalDiffTime
  , totalDuration :: NominalDiffTime
  , maximumROI :: ROI a
  , minimumROI :: ROI a
  , meanROI :: ROI a
  , stdDevROI :: ROI a
  , totalROI :: ROI a
  } deriving (Show)


stats2list :: (Show a) => Stats a -> [[String]]
stats2list stats =
  ["Count", show (count stats)]
  
  : ["Longest duration", TL.unpack (F.format (FT.days 1) (longestDuration stats)) ++ "d"]
  : ["Shortest duration", TL.unpack (F.format (FT.days 1) (shortestDuration stats)) ++ "d"]
  : ["Mean duration", TL.unpack (F.format (FT.days 1) (meanDuration stats)) ++ "d"]
  : ["StdDev. duration", TL.unpack (F.format (FT.days 1) (stdDevDuration stats)) ++ "d"]
  : ["Total duration", TL.unpack (F.format (FT.days 1) (totalDuration stats)) ++ "d"]
  : ["Maximum ROI", show (unROI (maximumROI stats))]
  : ["Minimum ROI", show (unROI (minimumROI stats))]
  : ["Mean ROI", show (unROI (meanROI stats))]
  : ["StdDev. ROI (ca.)", show (unROI (stdDevROI stats))]
  : ["Total ROI", show (unROI (totalROI stats))]

  : []

roiHelper ::
  (Fractional a, Real a) => Vector (NominalDiffTime, a) -> Maybe (Stats a)
roiHelper vs =
  let ts = Vec.map fst vs
      tsReal = Vec.map realToFrac ts
      xs = Vec.map snd vs
      xsLog = Vec.map (log . realToFrac) xs
  in case Vec.null vs of
       True -> Nothing
       False -> Just $ Stats {
         count = Vec.length vs
         , longestDuration = Vec.maximum ts
         , shortestDuration = Vec.minimum ts
         , meanDuration = realToFrac (Sample.mean tsReal)
         , stdDevDuration = realToFrac (Sample.stdDev tsReal)
         , totalDuration = Vec.sum ts
         , maximumROI = ROI (Vec.maximum xs)
         , minimumROI = ROI (Vec.minimum xs)
         , meanROI = ROI (realToFrac (exp (Sample.mean xsLog)))
         , stdDevROI = ROI (realToFrac (exp (Sample.stdDev xsLog)))
         , totalROI = ROI (product xs)
         }


statistics ::
  (Functor longOrShort, Functor notInv, Functor inv, Fractional a, Real a)
  => longOrShort (Yield (notInv [Vector (UTCTime, a)], inv [Vector (UTCTime, a)]))
  -> longOrShort (Yield (notInv (Maybe (Stats a)), inv (Maybe (Stats a))))
statistics =
  let f v = biliftA2 (\t0 tn -> tn `diffUTCTime` t0) (\_ x -> x) (Vec.head v) (Vec.last v)
      g = Vec.fromList . map f
  in fmap (fmap (bimap (fmap (roiHelper . g)) (fmap (roiHelper . g))))
