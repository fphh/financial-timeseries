
module FinancialTimeseries.Algorithm.MonteCarlo where

import Data.Distributive (Distributive, distribute)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Vector.Algorithms.Merge as Merge

import qualified System.Random as R

import qualified Statistics.Sample as Sample

import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Table (Table(..))
import FinancialTimeseries.Type.Types (Equity(..), Yield(..), AbsoluteDrawdown(..), RelativeDrawdown(..), Invested(..), NotInvested(..), unswapYieldInvested)
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

{-
mc ::
  (Num a, Functor longOrShort, Distributive longOrShort) =>
  Evaluate longOrShort t a
  -> Config
  -> Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (longOrShort (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))
mc evaluate cfg eqty xs = do
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = biliftA k k . unzip
  return (fmap (MonteCarlo . h . unEquity) ws)
-}

mc ::
  (Num a, Functor longOrShort, Distributive longOrShort) =>
  Config
  -> Equity a
  -> longOrShort (Yield (NotInvested [Vector (t, a)], Invested [Vector (t, a)]))
  -> IO (Evaluate longOrShort t a -> longOrShort (MonteCarlo (NotInvested (Vector (Vector a)), Invested (Vector (Vector a)))))
mc cfg eqty xs = do
  print "i"
  ss <- samples (sampleLength cfg) xs
  let n = numberOfSamples cfg
      g = fmap (Vec.map snd)
      ws evaluate = fmap distribute (distribute (map (fmap (fmap (biliftA g g)) . evaluate eqty) ss))
      k = distribute . Vec.fromList . take n
      h = biliftA k k . unzip
  return (\e -> fmap (MonteCarlo . h . unEquity) (ws e))


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


stats2list :: [Stats a] -> [Table a]
stats2list xs =
  let qheaders = ["Q05", "Q25", "Q50", "Q75", "Q95"]
      pheaders = ["P(X < 0.5)", "P(X < 0.75)", "P(X < 1.0)", "P(X < 1.25)", "P(X < 1.5)", "P(X < 1.75)", "P(X < 2.0)"]
      mheaders = ["Max.", "Min.", "Mean", "StdDev."]
  in Table "Quantiles" qheaders (map (row . quantiles) xs)
     : Table "Moments" mheaders (map (row . moments) xs)
     : Table "Probabilities" pheaders (map (row . probabilities) xs)
     : []


mcYields ::
  (Fractional a) =>
  Vector (Vector a) -> Yield (Vector a)
mcYields =
  Yield
  . Vec.map (\vs -> Vec.last vs / Vec.head vs)

mcAbsoluteDrawdowns ::
  (Ord a, Fractional a) =>
  Vector (Vector a) -> AbsoluteDrawdown (Vector a)
mcAbsoluteDrawdowns =
  AbsoluteDrawdown
  . Vec.map (\vs -> Vec.minimum vs / Vec.head vs)

mcRelativeDrawdowns ::
  (Ord a, Fractional a) =>
  Vector (Vector a) -> RelativeDrawdown (Vector a)
mcRelativeDrawdowns =
  RelativeDrawdown
  . Vec.map (\v -> Vec.minimum (Vec.zipWith (/) v (Vec.postscanl max 0 v)))

toStatistics ::
  (Functor g, Distributive f, Real a, Fractional a) =>
  (Vector (Vector a) -> f (Vector a)) -> g [Vector (Vector a)] -> g (f [Table a])
toStatistics f = fmap (fmap (stats2list . map mkStatistics) . distribute . map f)

yields ::
  (Fractional a, Real a) =>
  MonteCarlo (NotInvested [Vector (Vector a)], Invested [Vector (Vector a)])
  -> MonteCarlo (Yield (NotInvested [Table a], Invested [Table a]))
yields = fmap (unswapYieldInvested . biliftA (toStatistics mcYields) (toStatistics mcYields))

{-
yields ::
  (Fractional a, Real a) =>
  MonteCarlo [Vector (Vector a)] -> MonteCarlo (Yield [Table a])
yields = toStatistics mcYields

absoluteDrawdowns ::
  (Fractional a, Real a) =>
  MonteCarlo [Vector (Vector a)] -> MonteCarlo (AbsoluteDrawdown [Table a])
absoluteDrawdowns = toStatistics mcAbsoluteDrawdowns

relativeDrawdowns ::
  (Fractional a, Real a) =>
  MonteCarlo [Vector (Vector a)] -> MonteCarlo (RelativeDrawdown [Table a])
relativeDrawdowns = toStatistics mcRelativeDrawdowns
-}
