{-# LANGUAGE DeriveFunctor #-}



module FinancialTimeseries.Type.Evaluate where

import Data.Time (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)


import FinancialTimeseries.Type.Invested (Invested(..), NotInvested(..))
import FinancialTimeseries.Type.Types(Equity(..), Yield(..), swapYieldInvested, swapInvestedEquity)
import FinancialTimeseries.Util.Pretty (Pretty, pretty)
import FinancialTimeseries.Util.Util (biliftA)

profit ::
  (Fractional a) =>
  (a -> a -> a)
  -> (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
profit p =
  let f v =
        let (_, x0) = Vec.head v
        in Vec.map (fmap (p x0)) v
  in Yield . biliftA (fmap (map f)) (fmap (map f))



newtype Long a = Long {
  unLong :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Long a) where
  pretty (Long x) = "Long\n" ++ pretty x


long ::
  (Fractional a)
  => (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Long (Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
long xs = Long (profit (\x0 xn -> xn/x0) xs)


newtype Short a = Short {
  unShort :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Short a) where
  pretty (Short x) = "Short\n" ++ pretty x

short ::
  (Fractional a)
  => (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Short (Yield (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)]))
short xs = Short (profit (\x0 xn -> (x0-xn)/x0) xs)


longEvaluate :: (Num a) => Equity a -> Yield [Vector (t, a)] -> Equity (Vector (t, a))
longEvaluate (Equity start) =
  let go _ [] = []
      go x (v:vs) =
        let u = Vec.map (fmap (x*)) v
        in u : go (snd (Vec.last u)) vs
  in Equity . Vec.concat . go start . unYield


class Evaluate longOrShort where
  evaluate ::
    (Num a) =>
    Equity a
    -> longOrShort (Yield (NotInvested [Vector (u, a)], Invested [Vector (u, a)]))
    -> longOrShort (Equity (NotInvested (Vector (u, a)), Invested (Vector (u, a))))


instance Evaluate Long where
  evaluate eqty =
    let eval = longEvaluate eqty
    in fmap (swapInvestedEquity . biliftA (fmap eval) (fmap eval) . swapYieldInvested)


evaluateInvested ::
  (Num a, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (Yield (NotInvested [Vector (u, a)], Invested [Vector (u, a)]))
  -> longOrShort (Equity (Invested (Vector (u, a))))
evaluateInvested eqty = fmap (fmap snd) . evaluate eqty


evaluateNotInvested ::
  (Num a, Functor longOrShort, Evaluate longOrShort) =>
  Equity a
  -> longOrShort (Yield (NotInvested [Vector (u, a)], Invested [Vector (u, a)]))
  -> longOrShort (Equity (NotInvested (Vector (u, a))))
evaluateNotInvested eqty = fmap (fmap fst) . evaluate eqty


