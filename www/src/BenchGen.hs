module Main where

import Control.Monad
import Data.List
import System.Environment
import BenchUtils
import CSV
import System.FilePath

putGroup (BenchGroup nn rs comps) = do
    putStrLn (intercalate "-" nn)
    forM rs $ \(dat, r) -> do
        putStr "  "
        putStr (row 60 (benchName r))
        putStrLn (printAutoScale $ mean r)
    forM comps $ \(dat, cs) -> do
        putStrLn ("  " ++ dat)
        let range = getRange $ minimum $ map (mean . snd) cs
        forM cs $ \(t, r) -> do
            putStr $ "    " ++ (row 58 t)
            putStrLn (printScale range $ mean r)
  where
    row n s
        | length s < n = s ++ replicate (n - length s) ' '
        | otherwise    = s

main = do
    a <- getArgs
    case a of
        "generate":file:[] -> do
            putStrLn "generating .."
            let bn = toBenchmarksName $ takeBaseName file
            putStrLn $ show bn

            allCsv <- parseCSV <$> readFile file

            mapM_ putGroup $ groupBench $ csvRows allCsv

        _ ->
            return ()
