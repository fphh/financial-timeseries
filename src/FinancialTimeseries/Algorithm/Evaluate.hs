
module FinancialTimeseries.Algorithm.Evaluate where


import Data.Time (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Short (Short(..))
import FinancialTimeseries.Type.Types(Invested(..), NotInvested(..), Equity(..), Yield(..), Price(..), swapYieldInvested, swapInvestedEquity)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.Util (biliftA)

profit ::
  (Fractional a) =>
  (a -> a -> a)
  -> Price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
profit p =
  let f v =
        let (_, x0) = Vec.head v
        in Vec.map (fmap (p x0)) v
      g = fmap (map f)
  in Yield . biliftA g g . unPrice

long ::
  (Fractional a)
  => Price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Long (Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
long = Long . profit (\x0 xn -> xn/x0)


short ::
  (Fractional a)
  => Price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Short (Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
short = Short . profit (\x0 xn -> (x0-xn)/x0)


longEvaluate :: (Num a) => Equity a -> Yield [Vector (t, a)] -> Equity (Vector (t, a))
longEvaluate (Equity start) =
  let go _ [] = []
      go x (v:vs) =
        let u = Vec.map (fmap (x*)) v
        in u : go (snd (Vec.last u)) vs
  in Equity . Vec.concat . go start . unYield


class Evaluate longOrShort where
  evaluate ::
    (Num a, Functor notInv, Functor inv) =>
    Equity a
    -> longOrShort (Yield (notInv [Vector (t, a)], inv [Vector (t, a)]))
    -> longOrShort (Equity (notInv (Vector (t, a)), inv (Vector (t, a))))


instance Evaluate Long where
  evaluate eqty =
    let eval = longEvaluate eqty
    in fmap (swapInvestedEquity . biliftA (fmap eval) (fmap eval) . swapYieldInvested)


evaluateInvested ::
  (Num a, Functor notInv, Functor inv, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (Yield (notInv [Vector (u, a)], inv [Vector (u, a)]))
  -> longOrShort (Equity (inv (Vector (u, a))))
evaluateInvested eqty = fmap (fmap snd) . evaluate eqty


evaluateNotInvested ::
  (Num a, Functor notInv, Functor inv, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (Yield (notInv [Vector (u, a)], inv [Vector (u, a)]))
  -> longOrShort (Equity (notInv (Vector (u, a))))
evaluateNotInvested eqty = fmap (fmap fst) . evaluate eqty


