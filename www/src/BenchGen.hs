module Main where

import System.Environment
import CSV

main = do
    a <- getArgs
    case a of
        "generate":file:[] -> do
            allCsv <- parseCSV <$> readFile file
            putStrLn $ show $ map benchName $ csvRows allCsv
        _ ->
            return ()
