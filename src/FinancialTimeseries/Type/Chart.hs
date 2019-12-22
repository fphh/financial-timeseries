{-# LANGUAGE DeriveFunctor #-}

module FinancialTimeseries.Type.Chart where

import Data.Vector (Vector)

import qualified Graphics.Rendering.Chart.Easy as E

import FinancialTimeseries.Type.Labeled (Labeled)

-- type CurveParameters l = E.EC l ()


data ParaCurve c x a = ParaCurve {
  parameters :: E.EC (E.Layout x a) ()
  , curve :: [c (x, a)]
  } -- deriving (Functor)


data Chart params curve = Chart {
  title :: String
  , curves :: [Labeled params curve]
  } deriving (Show)


add :: Labeled params curve -> Chart params curve -> Chart params curve
add lbld c = c {
  curves = curves c ++ [lbld]
  }

type LChart params x a = Chart params [Vector (x, a)]

type LPChart params x a = Chart params (ParaCurve Vector x a)
