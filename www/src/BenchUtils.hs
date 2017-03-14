module BenchUtils
    ( Range(..)
    , getRange
    , rangeToHName
    , rangeToFullHName
    , scale
    , autoScale
    , printScale
    , printAutoScale
    , BenchmarksName(..)
    , toBenchmarksName
    , BenchCat(..)
    , toBenchCat
    , BenchResult(..)
    , BenchGroup(..)
    , groupBench
    ) where

import Utils
import Data.List (isSuffixOf, sort, nub, partition)
import Data.Maybe (isJust)
import Data.Bifunctor

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
rangeToHName RMicroSeconds = "μs"
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

printScale :: Range -> Double -> String
printScale r d = show (scale r d) ++ " " ++ rangeToHName r

printAutoScale :: Double -> String
printAutoScale d = show (autoScale d) ++ " " ++ rangeToHName (getRange d)

data BenchmarksName = BenchmarksName
    { benchmarksNameRaw         :: String
    , benchmarksNameGHCVersion  :: [Int]
    , benchmarksNameFndVersion  :: [Int]
    , benchmarksNameGitAfterTag :: Int
    , benchmarksNameGitHash     :: String
    } deriving (Show)

-- | Take something a filename (with optional .csv suffix) and turn it into a versioned benchmark name
--
-- > toBenchName "7.10.3-foundation-v0.0.1-116-gf21d194.csv"
-- BenchName "7.10.3-foundation-v0.0.1-116-gf21d194.csv" [7,10,3] [0,0,1] 116 "gf21d194"
--
toBenchmarksName :: String -> Maybe BenchmarksName
toBenchmarksName filename
    | isSuffixOf ".csv" filename = toStruct $ take (length filename - 4) filename
    | otherwise                  = toStruct filename
  where
    toStruct s =
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
    } deriving (Show,Eq)

toBenchCat :: String -> Maybe BenchCat
toBenchCat s =
    case wordsWhen (== '/') s of
        "types":ty:fct:dat:comp:[] -> Just $ BenchCatType ty fct dat (Just comp)
        "types":ty:fct:dat:[]      -> Just $ BenchCatType ty fct dat Nothing
        _                          -> Nothing

data BenchGroup = BenchGroup
    { benchGroupName         :: [String]
    , benchGroupResults      :: [(String, BenchResult)]
    , benchGroupComparaisons :: [(String, [(String, BenchResult)])]
    }
    deriving (Show,Eq)

groupBench :: [BenchResult] -> [BenchGroup]
groupBench allbrs =
    concatMap (\ty -> toHighGroup ty $ filter ((==) ty . benchCatType . fst) allBenchs) allTypes
  where
    toHighGroup ty brs =
        let allFcts = nub $ sort $ map (benchCatTypeFct . fst) brs
         in map (\fct -> toBenchGroup [ty,fct] $ filter ((==) fct . benchCatTypeFct . fst) brs) allFcts

    allTypes = nub $ sort $ map benchCatType allBenchCat
    allBenchCat = map (maybe (error "bench cat unreadable") id . toBenchCat . benchName) allbrs
    allBenchs = zip allBenchCat allbrs

    toBenchGroup groupName res =
        BenchGroup groupName (map (first benchCatTypeData) normalBenches)
                             (compGroups)
      where
        (compBenches, normalBenches) = partition (isJust . benchCatTypeComp . fst) res
        allCompData = nub $ sort $ map (benchCatTypeData . fst) compBenches
        compGroups = map (\c -> (c, map (first (maybe (error "xx") id . benchCatTypeComp)) $ filter ((==) c . benchCatTypeData . fst) compBenches)
                         ) allCompData

data BenchResult = BenchResult
    { benchName :: String
    , mean      :: Double
    , meanLB    :: Double
    , meanUB    :: Double
    , stdDev    :: Double
    , stdDevLB  :: Double
    , stdDevUB  :: Double
    } deriving (Show,Eq)
