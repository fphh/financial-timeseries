{-# LANGUAGE ScopedTypeVariables #-}


module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import Data.Bifunctor (bimap)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified System.Random as R

import FinancialTimeseries.Statistics.Statistics (Stats(..), mkStatistics, yield, absoluteDrawdown, relativeDrawdown)
import FinancialTimeseries.Type.Chart (Chart(..))
import FinancialTimeseries.Type.Fraction (Fraction)
import FinancialTimeseries.Type.Labeled (Labeled(..))
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Table (Cell(..), Table(..), row)
import FinancialTimeseries.Type.Types (Equity(..), TradeYield(..), TimeseriesYield(..), AbsoluteDrawdown(..), RelativeDrawdown(..), Invested(..), NotInvested(..))
import FinancialTimeseries.Util.DistributivePair (DistributivePair, undistributePair)


sample ::
  (Functor longOrShort, Functor inv, Functor notInv) =>
  longOrShort (TradeYield (notInv [Vector u], inv [Vector u]))
  -> [Int]
  -> [Int]
  -> longOrShort (TradeYield (notInv [Vector u], inv [Vector u]))
sample xs as bs =
  let g rs us =
        let zs = Vec.fromList us
            len = Vec.length zs
        in map ((zs Vec.!) . (`mod` len)) rs
  in fmap (fmap (bimap (fmap (g as)) (fmap (g bs)))) xs


data Config gen a = Config {
  sampleLength :: Int
  , numberOfSamples :: Int
  , startEquity :: Equity a
  , fractions :: [Fraction a]
  , randomGenerator :: gen
  } deriving (Show)


newMCConfig :: Int -> Int -> Equity a -> [Fraction a] -> IO (Config R.StdGen a)
newMCConfig len num eqty fs = R.newStdGen >>= return . Config len num eqty fs

  
samples ::
  (R.RandomGen gen, Functor longOrShort, Functor inv, Functor notInv) =>
  (gen, gen)
  -> Int
  -> longOrShort (TradeYield (notInv [Vector (t, a)], inv [Vector (t, a)]))
  -> [longOrShort (TradeYield (notInv [Vector (t, a)], inv [Vector (t, a)]))]
samples (ga, gb) sampLen xs =
  let chunk = (\(us, vs) -> us : chunk vs) . splitAt sampLen
      as = chunk (R.randoms ga)
      bs = chunk (R.randoms gb)
  in (zipWith (sample xs) as bs)


type Evaluate longOrShort t a =
  Equity a
  -> longOrShort (TradeYield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> longOrShort (Equity (NotInvested (Vector (t, a)), Invested (Vector (t, a))))

mc ::
  (R.RandomGen gen, Num a, Functor longOrShort, Distributive longOrShort) =>
  Config gen a
  -> longOrShort (TradeYield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> (params -> Evaluate longOrShort t a -> longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a))))))
mc cfg xs =
  let ss = samples (R.split (randomGenerator cfg)) (sampleLength cfg) xs
      n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      eqty = startEquity cfg
      ws evaluate = fmap distribute (distribute (map (fmap (fmap (bimap g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = bimap k k . unzip
  in (\p e -> fmap (Labeled p . MonteCarlo . h . unEquity) (ws e))



stats2list :: [Labeled params (Stats a)] -> [Table params a]
stats2list xs =
  let qheaders = map CString ["Q05", "Q25", "Q50", "Q75", "Q95", "Sample Size"]
      pheaders = map CString ["P(X < 0.5)", "P(X < 0.75)", "P(X < 1.0)", "P(X < 1.25)", "P(X < 1.5)", "P(X < 1.75)", "P(X < 2.0)", "Sample Size"]
      mheaders = map CString ["Max.", "Min.", "Mean", "StdDev.", "Sample Size"]
      mkRow g x = row (g x) ++ [CInt (sampleSize x)]
  in Table "Quantiles" qheaders (map (fmap (mkRow quantiles)) xs)
     : Table "Moments" mheaders (map (fmap (mkRow moments)) xs)
     : Table "Probabilities" pheaders (map (fmap (mkRow probabilities)) xs)
     : []

stats2cdfChart :: [Labeled params (Stats a)] -> Chart params Double a
stats2cdfChart = Chart "CDF" . map (fmap ((:[]) . cdf))

metrics ::
  (Distributive f, Distributive g, DistributivePair g, Fractional b, Real b) =>
  (Labeled params a -> g (Labeled params (Vector b)))
  -> [f (Labeled params (MonteCarlo (NotInvested a, Invested a)))]
  -> f (MonteCarlo (g (NotInvested (Chart params Double b, [Table params b]), Invested (Chart params Double b, [Table params b]))))
metrics mcf =
  let k xs = 
        let ys = fmap (map (fmap mkStatistics)) (distribute (map mcf xs))
        in undistributePair (fmap stats2cdfChart ys, fmap stats2list ys) 

      f (Labeled p x) = bimap (distribute . Labeled p) (distribute . Labeled p) x
      g = fmap k . distribute
      h = undistributePair . bimap (distribute . g) (distribute . g) . unzip . fmap f
  in fmap (fmap h . distribute . map distribute) . distribute


timeseriesYields ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (TimeseriesYield (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
timeseriesYields = metrics (TimeseriesYield . fmap (Vec.map yield))

absoluteDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (AbsoluteDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
absoluteDrawdowns = metrics (AbsoluteDrawdown . fmap (Vec.map absoluteDrawdown))

relativeDrawdowns ::
  (Distributive longOrShort, Real a, Fractional a) =>
  [longOrShort (Labeled params (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))]
  -> longOrShort (MonteCarlo (RelativeDrawdown (NotInvested (Chart params Double a, [Table params a]), Invested (Chart params Double a, [Table params a]))))
relativeDrawdowns = metrics (RelativeDrawdown . fmap (Vec.map relativeDrawdown))
