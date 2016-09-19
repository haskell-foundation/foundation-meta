module BenchUtils
    ( Range(..)
    , getRange
    , rangeToHName
    , rangeToFullHName
    , scale
    , autoScale
    , BenchmarksName(..)
    , toBenchmarksName
    , BenchCat(..)
    , toBenchCat
    ) where

import Utils
import Data.List (isSuffixOf)

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

data BenchmarksName = BenchmarksName
    { benchmarksNameRaw         :: String
    , benchmarksNameGHCVersion  :: [Int]
    , benchmarksNameFndVersion  :: [Int]
    , benchmarksNameGitAfterTag :: Int
    , benchmarksNameGitHash     :: String
    }

instance Show BenchmarksName where
    show bn = benchmarksNameRaw bn

-- | Take something a filename (with optional .csv suffix) and turn it into a versioned benchmark name
--
-- > toBenchName "7.10.3-foundation-v0.0.1-116-gf21d194.csv"
-- BenchName "7.10.3-foundation-v0.0.1-116-gf21d194.csv" [7,10,3] [0,0,1] 116 "gf21d194"
--
toBenchmarksName :: String -> Maybe BenchmarksName
toBenchmarksName filename
    | isSuffixOf ".csv" filename = toBenchmarksName $ take (length filename - 4) filename
    | otherwise                  = toBenchmarksName filename
  where
    toBenchName s =
        case wordsWhen (== '-') s of
            ghcVer:"foundation":('v':fndVer):gitCommits:hash:[] ->
                Just $ BenchmarksName s (toVer ghcVer) (toVer fndVer) (toInt gitCommits) hash
            _                                                   -> Nothing

    toVer v = map toInt $ wordsWhen (== '.') v
    toInt :: String -> Int
    toInt = read

data BenchCat = BenchCatType
    { benchCatType     :: String
    , benchCatTypeFct  :: String
    , benchCatTypeData :: String
    , benchCatTypeComp :: Maybe String
    }

toBenchCat :: String -> Maybe BenchCat
toBenchCat s =
    case wordsWhen (== '/') s of
        "types":ty:fct:dat:comp:[] -> Just $ BenchCatType ty fct dat (Just comp)
        "types":ty:fct:dat:[]      -> Just $ BenchCatType ty fct dat Nothing
        _                          -> Nothing
