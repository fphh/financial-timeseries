{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import Data.Bifunctor (bimap)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified System.Random as R

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Statistics.Statistics (Stats(..), mkStatistics)
import FinancialTimeseries.Type.Chart (Chart(..))
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Table (Table(..))
import FinancialTimeseries.Type.Types (Equity(..), Yield(..), AbsoluteDrawdown(..), RelativeDrawdown(..), Invested(..), NotInvested(..))
import FinancialTimeseries.Util.DistributivePair (DistributivePair, undistributePair)
import FinancialTimeseries.Util.Row (row)



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
  in fmap (fmap (bimap (fmap (g as)) (fmap (g bs)))) xs


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
      ws evaluate = fmap distribute (distribute (map (fmap (fmap (bimap g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = bimap k k . unzip
  return (\p e -> fmap (Labeled p . MonteCarlo . h . unEquity) (ws e))



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
  let f (Labeled p x) = bimap (distribute . Labeled p) (distribute . Labeled p) x
      g = fmap statistics . distribute
      h = undistributePair . bimap (distribute . g) (distribute . g) . unzip . fmap f
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
