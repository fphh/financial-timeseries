{-# LANGUAGE DeriveFunctor #-}


module FinancialTimeseries.Type.Profit where

import Data.Time (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)


import FinancialTimeseries.Type.Invested (Invested(..), NotInvested(..))
import FinancialTimeseries.Type.Pretty (Pretty, pretty)
import FinancialTimeseries.Type.Util (biliftA)


profit ::
  (Fractional a) =>
  (a -> a -> a)
  -> (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
profit p =
  let f v =
        let (_, x0) = Vec.head v
        in Vec.map (fmap (p x0)) v
  in biliftA (fmap (map f)) (fmap (map f))



newtype Long a = Long {
  unLong :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Long a) where
  pretty (Long x) = "Long\n" ++ pretty x

long ::
  (Fractional a)
  => (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Long (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
long xs = Long (profit (\x0 xn -> xn/x0) xs)


newtype Short a = Short {
  unShort :: a
  } deriving (Show, Functor)

instance Pretty a => Pretty (Short a) where
  pretty (Short x) = "Short\n" ++ pretty x

short ::
  (Fractional a)
  => (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
  -> Short (NotInvested [Vector (UTCTime, a)], Invested [Vector (UTCTime, a)])
short xs = Short (profit (\x0 xn -> (x0-xn)/x0) xs)


longEvaluate :: (Num a) => a -> [Vector (t, a)] -> Vector (t, a)
longEvaluate start =
  let go _ [] = []
      go x (v:vs) =
        let u = Vec.map (fmap (x*)) v
        in u : go (snd (Vec.last u)) vs
  in Vec.concat . go start


class Evaluate longOrShort where
  evaluate ::
    (Num a, Functor notInv, Functor inv) =>
    a -> longOrShort (notInv [Vector (u, a)], inv [Vector (u, a)]) -> longOrShort (notInv (Vector (u, a)), inv (Vector (u, a)))

instance Evaluate Long where
  evaluate start = fmap (biliftA (fmap (longEvaluate start)) (fmap (longEvaluate start)))



evaluateInvested ::
  (Num a, Functor notInv, Functor inv, Functor longOrShort, Evaluate longOrShort) =>
  a -> longOrShort (notInv [Vector (u, a)], inv [Vector (u, a)]) -> longOrShort (inv (Vector (u, a)))
evaluateInvested start = fmap snd . evaluate start

evaluateNotInvested ::
  (Num a, Functor notInv, Functor inv, Functor longOrShort, Evaluate longOrShort) =>
  a -> longOrShort (notInv [Vector (u, a)], inv [Vector (u, a)]) -> longOrShort (notInv (Vector (u, a)))
evaluateNotInvested start = fmap fst . evaluate start


