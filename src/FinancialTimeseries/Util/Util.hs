

module FinancialTimeseries.Util.Util where


biliftA :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
biliftA f g (u, v) = (f u, g v)

biliftA2 :: (a -> c -> e) -> (b -> d -> f) -> (a, b) -> (c, d) -> (e, f)
biliftA2 f g (u, v) (x, y) = (f u x, g v y)
