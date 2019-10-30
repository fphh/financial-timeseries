

module FinancialTimeseries.Render.Render where

import Text.Blaze.Html (Html)

import FinancialTimeseries.Type.Long (Long(..))
import FinancialTimeseries.Type.MonteCarlo (MonteCarlo(..))
import FinancialTimeseries.Type.Types (Invested(..), NotInvested(..), Equity(..), Yield(..), Price(..), AbsoluteDrawdown(..), RelativeDrawdown(..))

import FinancialTimeseries.Type.Short (Short(..))

class Render a where
  render :: [String] -> a -> Html


instance (Render a) => Render (MonteCarlo a) where
  render xs (MonteCarlo m) = render (xs ++ ["MonteCarlo"]) m

instance (Render a) => Render (Invested a) where
  render xs (Invested m) = render (xs ++ ["Invested"]) m

instance (Render a) => Render (NotInvested a) where
  render xs (NotInvested m) = render (xs ++ ["NotInvested"]) m

instance (Render a) => Render (Equity a) where
  render xs (Equity m) = render (xs ++ ["Equity"]) m

instance (Render a) => Render (Yield a) where
  render xs (Yield m) = render (xs ++ ["Yield"]) m

instance (Render a) => Render (Price a) where
  render xs (Price m) = render (xs ++ ["Price"]) m

instance (Render a) => Render (AbsoluteDrawdown a) where
  render xs (AbsoluteDrawdown m) = render (xs ++ ["AbsoluteDrawdown"]) m

instance (Render a) => Render (RelativeDrawdown a) where
  render xs (RelativeDrawdown m) = render (xs ++ ["RelativeDrawdown"]) m

instance (Render a) => Render [a] where
  render xs ys = mapM_ (render xs) ys

instance (Render a, Render b) => Render (a, b) where
  render xs (u, v) = render xs u <> render xs v

instance (Render a) => Render (Long a) where
  render xs (Long m) = render (xs ++ ["Long"]) m

instance (Render a) => Render (Short a) where
  render xs (Short m) = render (xs ++ ["Short"]) m
