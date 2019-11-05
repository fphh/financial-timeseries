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

stats2pdfChart :: [Labeled params (Stats a)] -> Chart params Double a
stats2pdfChart = Chart "pdf" . map (fmap ((:[]) . pdf))

metrics ::
  (Distributive f, Distributive g, DistributivePair g, Fractional b, Real b) =>
  (Labeled params a -> g (Labeled params (Vector b)))
  -> [f (Labeled params (MonteCarlo (NotInvested a, Invested a)))]
  -> f (MonteCarlo (g (NotInvested (Chart params Double b, [Table params b]), Invested (Chart params Double b, [Table params b]))))
metrics mcf =
  let k xs = 
        let ys = fmap (map (fmap mkStatistics)) (distribute (map mcf xs))
        in undistributePair (fmap stats2pdfChart ys, fmap stats2list ys) 

      f (Labeled p x) = bimap (distribute . Labeled p) (distribute . Labeled p) x
      g = fmap k . distribute
      h = undistributePair . bimap (distribute . g) (distribute . g) . unzip . fmap f
  in fmap (fmap h . distribute . map distribute) . distribute



mcYields ::
  (Fractional a) =>
  Labeled params (Vector (Vector a)) -> Yield (Labeled params (Vector a))
mcYields (Labeled lbl xs) =
  Yield
  $ Labeled lbl
  $ Vec.map (\vs -> Vec.last vs / Vec.head vs) xs

yields ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (Yield (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
yields = metrics mcYields



mcAbsoluteDrawdowns ::
  (Ord a, Fractional a) =>
  Labeled params (Vector (Vector a)) -> AbsoluteDrawdown (Labeled params (Vector a))
mcAbsoluteDrawdowns (Labeled lbl xs) =
  AbsoluteDrawdown
  $ Labeled lbl
  $ Vec.map (\vs -> Vec.minimum vs / Vec.head vs) xs

absoluteDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (AbsoluteDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
absoluteDrawdowns = metrics mcAbsoluteDrawdowns



mcRelativeDrawdowns ::
  (Ord a, Fractional a) =>
  Labeled params (Vector (Vector a)) -> RelativeDrawdown (Labeled params (Vector a))
mcRelativeDrawdowns (Labeled lbl xs) =
  RelativeDrawdown
  $ Labeled lbl
  $ Vec.map (\v -> Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v))) xs

relativeDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (RelativeDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
relativeDrawdowns = metrics mcRelativeDrawdowns
