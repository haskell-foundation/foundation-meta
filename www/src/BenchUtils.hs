module BenchUtils
    ( Range(..)
    , getRange
    , rangeToHName
    , rangeToFullHName
    , scale
    , autoScale
    ) where

-- | the best range associated with a (Double) time value in seconds
data Range = RSeconds | RMilliSeconds | RMicroSeconds | RNanoSeconds

-- | Return the range display associated with a time value in seconds
getRange :: Double -> Range
getRange d
    | d < 0.000002 = RNanoSeconds
    | d < 0.002    = RMicroSeconds
    | d < 1.5      = RMilliSeconds
    | otherwise    = RSeconds

-- | Return the short name for the range
rangeToHName :: Range -> String
rangeToHName RSeconds      = "s"
rangeToHName RMilliSeconds = "ms"
rangeToHName RMicroSeconds = "us"
rangeToHName RNanoSeconds  = "ns"

-- | Return the long name for the range
rangeToFullHName :: Range -> String
rangeToFullHName RSeconds      = "seconds"
rangeToFullHName RMilliSeconds = "milliseconds"
rangeToFullHName RMicroSeconds = "microseconds"
rangeToFullHName RNanoSeconds  = "nanoseconds"

-- | Scale the value to the required precision range
--
-- > scale RMilliSeconds 1.2 = 1200.0
-- > scale RMicroSeconds 0.00002 = 20.0
scale :: Range -> Double -> Double
scale r d = fromIntegral (round (d * (10 ^ (n+prec))) :: Int) / (10^n)
  where
    (prec, n) = case r of
                    RSeconds      -> (0,1)
                    RMilliSeconds -> (3,0)
                    RMicroSeconds -> (6,0)
                    RNanoSeconds  -> (9,0)

autoScale :: Double -> Double
autoScale d = scale (getRange d) d
