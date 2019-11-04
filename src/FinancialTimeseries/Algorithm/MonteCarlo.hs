{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified System.Random as R

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.Chart (Chart(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Table (Table(..))
import FinancialTimeseries.Type.Types (Equity(..), Yield(..), AbsoluteDrawdown(..), RelativeDrawdown(..), Invested(..), NotInvested(..))
import FinancialTimeseries.Util.DistributivePair (DistributivePair, undistributePair)
import FinancialTimeseries.Util.Util (biliftA)



sample ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  longOrShort (Yield (notInv [Vector u], inv [Vector u]))
  -> [Int]
  -> [Int]
  -> longOrShort (Yield (notInv [Vector u], inv [Vector u]))
sample xs as bs =
  let g rs us =
        let zs = Vec.fromList us
            len = Vec.length zs
        in map ((zs Vec.!) . (`mod` len)) rs
  in fmap (fmap (biliftA (fmap (g as)) (fmap (g bs)))) xs


data Config = Config {
  sampleLength :: Int
  , numberOfSamples :: Int
  } deriving (Show)

samples ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  Int
  -> longOrShort (Yield (notInv [Vector (t, a)], inv [Vector (t, a)]))
  -> IO [longOrShort (Yield (notInv [Vector (t, a)], inv [Vector (t, a)]))]
samples sampLen xs = do
  (ga, gb) <- fmap R.split R.newStdGen

  let chunk = (\(us, vs) -> us : chunk vs) . splitAt sampLen
      as = chunk (R.randoms ga)
      bs = chunk (R.randoms gb)

  return (zipWith (sample xs) as bs)


type Evaluate longOrShort t a =
  Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> longOrShort (Equity (NotInvested (Vector (t, a)), Invested (Vector (t, a))))

mc ::
  (Num a, Functor longOrShort, Distributive longOrShort) =>
  Config
  -> Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (params -> Evaluate longOrShort t a -> longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a))))))
mc cfg eqty xs = do
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws evaluate = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = biliftA k k . unzip
  return (\p e -> fmap (Labeled p . MonteCarlo . h . unEquity) (ws e))


class Row a where
  row :: a x -> [x]

data Quantiles a = Quantiles {
  q05 :: !a
  , q25 :: !a
  , q50 :: !a
  , q75 :: !a
  , q95 :: !a
  } deriving (Show)

instance Row Quantiles where
  row (Quantiles a b c d e) = [a, b, c, d, e]

data Probabilities a = Probabilities {
  p0'50 :: !a
  , p0'75 :: !a
  , p1'00 :: !a
  , p1'25 :: !a
  , p1'50 :: !a
  , p1'75 :: !a
  , p2'00 :: !a
  } deriving (Show)

instance Row Probabilities where
  row (Probabilities a b c d e f g) = [a, b, c, d, e, f, g]

data Moments a = Moments {
  maxYield :: !a
  , minYield :: !a
  , meanYield :: !a
  , stdDevYield :: !a
  } deriving (Show)
  
instance Row Moments where
  row (Moments a b c d) = [a, b, c, d]

data Stats a = Stats {
  quantiles :: Quantiles a
  , probabilities :: Probabilities a
  , moments :: Moments a
  , pdf :: Vector (Double, a)
  } deriving (Show)

mkStatistics ::
  (Ord a, Num a, Fractional a, Real a) =>
  Vector a -> Stats a
mkStatistics vs =
  let noe = fromIntegral (Vec.length vs)
      sorted = Vec.modify Merge.sort vs
      sortedFrac = Vec.map realToFrac sorted

      quart s = sorted Vec.! (round (s * noe :: Double))
      q = Quantiles {
        q05 = quart 0.05
        , q25 = quart 0.25
        , q50 = quart 0.50
        , q75 = quart 0.75
        , q95 = quart 0.95
        }

      prob s = fromIntegral (Vec.length (Vec.takeWhile (<s) sorted)) / noe
      p = Probabilities {
        p0'50 = prob 0.5
        , p0'75 = prob 0.75
        , p1'00 = prob 1.0
        , p1'25 = prob 1.25
        , p1'50 = prob 1.5
        , p1'75 = prob 1.75
        , p2'00 = prob 2.00
        }

      m = Moments {
        maxYield = Vec.last sorted
        , minYield = Vec.head sorted
        , meanYield = realToFrac $ Sample.mean sortedFrac
        , stdDevYield = realToFrac $ Sample.stdDev sortedFrac
        }
        
  in Stats {
    quantiles = q
    , probabilities = p
    , moments = m
    , pdf = Vec.imap (\i x -> (fromIntegral i / noe, x)) sorted
    }


stats2list :: [Labeled params (Stats a)] -> [Table params a]
stats2list xs =
  let qheaders = ["Q05", "Q25", "Q50", "Q75", "Q95"]
      pheaders = ["P(X < 0.5)", "P(X < 0.75)", "P(X < 1.0)", "P(X < 1.25)", "P(X < 1.5)", "P(X < 1.75)", "P(X < 2.0)"]
      mheaders = ["Max.", "Min.", "Mean", "StdDev."]
  in Table "Quantiles" qheaders (map (fmap (row . quantiles)) xs)
     : Table "Moments" mheaders (map (fmap (row . moments)) xs)
     : Table "Probabilities" pheaders (map (fmap (row . probabilities)) xs)
     : []

toTable ::
  (Distributive f, Real a, Fractional a) =>
  (Labeled params (Vector (Vector a)) -> f (Labeled params (Vector a)))
  -> [Labeled params (Vector (Vector a))]
  -> f [Table params a]
toTable f = fmap (stats2list . map (fmap mkStatistics)) . distribute . map f



stats2pdfChart :: [Labeled params (Stats a)] -> Chart params Double a
stats2pdfChart = Chart "pdf" . map (fmap ((:[]) . pdf))

toChart ::
  (Distributive f, Fractional a, Real a) =>
  (Labeled params (Vector (Vector a)) -> f (Labeled params (Vector a)))
  -> [Labeled params (Vector (Vector a))]
  -> f (Chart params Double a)
toChart f = fmap (stats2pdfChart . map (fmap mkStatistics)) . distribute . map f

mcYields ::
  (Fractional a) =>
  Labeled params (Vector (Vector a)) -> Yield (Labeled params (Vector a))
mcYields (Labeled lbl xs) =
  Yield
  $ Labeled lbl
  $ Vec.map (\vs -> Vec.last vs / Vec.head vs) xs

mcAbsoluteDrawdowns ::
  (Ord a, Fractional a) =>
  Labeled params (Vector (Vector a)) -> AbsoluteDrawdown (Labeled params (Vector a))
mcAbsoluteDrawdowns (Labeled lbl xs) =
  AbsoluteDrawdown
  $ Labeled lbl
  $ Vec.map (\vs -> Vec.minimum vs / Vec.head vs) xs

mcRelativeDrawdowns ::
  (Ord a, Fractional a) =>
  Labeled params (Vector (Vector a)) -> RelativeDrawdown (Labeled params (Vector a))
mcRelativeDrawdowns (Labeled lbl xs) =
  RelativeDrawdown
  $ Labeled lbl
  $ Vec.map (\v -> Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v))) xs

metrics ::
  (Distributive longOrShort, Distributive f, DistributivePair f, Real a, Fractional a) =>
  ([Labeled params (Vector (Vector a))] -> f (chart params x a, [table params a]))
  -> [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (f (NotInvested (chart params x a, [table params a]), Invested (chart params x a, [table params a]))))
metrics statistics ms =
  let f (Labeled p x) = biliftA (distribute . Labeled p) (distribute . Labeled p) x
      g = fmap statistics . distribute
      h = undistributePair . biliftA (distribute . g) (distribute . g) . unzip . fmap f
  in fmap (fmap h . distribute . map distribute) (distribute ms)


yields ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (Yield (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
yields = metrics (undistributePair . (\x -> (toChart mcYields x, toTable mcYields x)))


absoluteDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (AbsoluteDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
absoluteDrawdowns = metrics (undistributePair . (\x -> (toChart mcAbsoluteDrawdowns x, toTable mcAbsoluteDrawdowns x)))

relativeDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (RelativeDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
relativeDrawdowns = metrics (undistributePair . (\x -> (toChart mcRelativeDrawdowns x, toTable mcRelativeDrawdowns x)))
