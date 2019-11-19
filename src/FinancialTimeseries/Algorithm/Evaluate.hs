
module FinancialTimeseries.Algorithm.Evaluate where

import Data.Bifunctor (bimap)
import Data.Distributive (Distributive, distribute)

import Data.Time (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import FinancialTimeseries.Type.Fraction (Fraction(..))
import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.Short (Short(..))
import FinancialTimeseries.Type.Types(Invested(..), NotInvested(..), Equity(..), TradeYield(..), Price(..), ExchangeRate(..))
import FinancialTimeseries.Util.DistributivePair (distributePair, undistributePair)


profitHelper ::
  (Fractional a) =>
  (a -> a -> a)
  -> (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> TradeYield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
profitHelper p =
  let f v =
        let (_, x0) = Vec.head v
        in Vec.map (fmap (p x0)) v
      g = fmap (map f)
  in TradeYield . bimap g g -- . unPrice


class Profit price where
  profit ::
    (Fractional a) =>
    (a -> a -> a)
    -> price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
    -> TradeYield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])

instance Profit Price where
  profit p = profitHelper p . unPrice

instance Profit ExchangeRate where
  profit p = profitHelper (\a b -> recip (p a b)) . unExchangeRate



long ::
  (Profit price, Fractional a)
  => price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Long (TradeYield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
long = Long . profit (\x0 xn -> xn/x0)


short ::
  (Profit price, Fractional a)
  => price (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Short (TradeYield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
short = Short . profit (\x0 xn -> (x0-xn)/x0)


longEvaluate :: (Num a) => Equity a -> TradeYield [Vector (t, a)] -> Equity (Vector (t, a))
longEvaluate (Equity start) =
  let go _ [] = []
      go x (v:vs) =
        let u = Vec.map (fmap (x*)) v
        in u : go (snd (Vec.last u)) vs
  in Equity . Vec.concat . go start . unTradeYield


longEvaluateFraction ::
  (Num a) =>
  Fraction a -> Equity a -> TradeYield [Vector (t, a)] -> Equity (Vector (t, a))
longEvaluateFraction (Fraction frac) (Equity start) =
  let go _ [] = []
      go x (v:vs) =
        let p = frac * x
            q = (1-frac) * x
            u = Vec.map (fmap ((q+) . (p*))) v
        in u : go (snd (Vec.last u)) vs
  in Equity . Vec.concat . go start . unTradeYield
 

class Evaluate longOrShort where
  evaluate ::
    (Num a, Distributive notInv, Distributive inv) =>
    Equity a
    -> longOrShort (TradeYield (notInv [Vector (t, a)], inv [Vector (t, a)]))
    -> longOrShort (Equity (notInv (Vector (t, a)), inv (Vector (t, a))))

  evaluateFraction ::
    (Num a, Distributive notInv, Distributive inv) =>
    Fraction a
    -> Equity a
    -> longOrShort (TradeYield (notInv [Vector (t, a)], inv [Vector (t, a)]))
    -> longOrShort (Equity (notInv (Vector (t, a)), inv (Vector (t, a))))


instance Evaluate Long where
  evaluate eqty =
    let eval = distribute . fmap (longEvaluate eqty) . distribute
    in fmap (undistributePair . bimap eval eval . distributePair)

  evaluateFraction frac eqty =
    let eval = distribute . fmap (longEvaluateFraction frac eqty) . distribute
    in fmap (undistributePair . bimap eval eval . distributePair)
   


evaluateInvested ::
  (Num a, Distributive notInv, Distributive inv, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (TradeYield (notInv [Vector (u, a)], inv [Vector (u, a)]))
  -> longOrShort (Equity (inv (Vector (u, a))))
evaluateInvested eqty = fmap (fmap snd) . evaluate eqty

evaluateNotInvested ::
  (Num a, Distributive notInv, Distributive inv, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (TradeYield (notInv [Vector (u, a)], inv [Vector (u, a)]))
  -> longOrShort (Equity (notInv (Vector (u, a))))
evaluateNotInvested eqty = fmap (fmap fst) . evaluate eqty


