--------------------------------------------------------------------------------
module CSV where

import           Data.Monoid
import           Text.Read

import           Data.List (intercalate, intersperse)
import           Generation
import           BenchUtils
import           Utils

-- | Typical row out of criterion CSVs
type Row = BenchResult

data CSV = CSV
    { csvHeader :: [String]
    , csvRows   :: [Row]
    }

csvToHtml :: CSV -> String
csvToHtml csv = intercalate "\n" $
    [ "<table class=\"table table-bordered table-hover table-condensed\">"
    , "<thead><tr><th>Name</th><th>Mean</th><th>Mean (Min)</th><th>Mean (Max)</th><th>StdDev</th><th>StdDev (Min)</th><th>StdDev (Max)</th></tr></thead>"
    , "<tbody>"
    ] ++ rows ++
    ["</tbody></table>"
    ]
  where
    rows = map toRow $ csvRows csv
    toRow (BenchResult bn m1 m2 m3 s1 s2 s3) =
        mconcat ["<tr>"
                , "<td>", bn, "</td>"
                , "<td>", prettyTime m1, "</td>"
                , "<td>", prettyTime m2, "</td>"
                , "<td>", prettyTime m3, "</td>"
                , "<td>", prettyTime s1, "</td>"
                , "<td>", prettyTime s2, "</td>"
                , "<td>", prettyTime s3, "</td>"
                , "</tr>"]
    prettyTime d = show (autoScale d) ++ rangeToHName (getRange d)

parseCSV :: String -> CSV
parseCSV content =
    case map (wordsWhen (== ',')) $ lines content of
        hdr:rows -> CSV hdr (map toRow rows)
  where
        toRow (n:m:ml:mu:s:sl:su:[]) =
            BenchResult n (rDouble m) (rDouble ml) (rDouble mu) (rDouble s) (rDouble sl) (rDouble su)
          where rDouble s =
                    case readMaybe s of
                        Nothing -> error ("couldn't read double : " ++ show s)
                        Just d  -> d
        toRow l = error ("wrong format of row: " ++ show l)

filterMap :: (String -> Maybe String)
          -> CSV
          -> CSV
filterMap f csv =
    CSV (csvHeader csv) (doFilter $ csvRows csv)
  where
    doFilter [] = []
    doFilter (r:rs) =
        case f (benchName r) of
            Nothing      -> doFilter rs
            Just newName -> r { benchName = newName } : doFilter rs

printCSV :: CSV -> String
printCSV content =
    intercalate "\n"
        ( toLine (csvHeader content)
        : map (toLine . printRow) (csvRows content)
        )
  where
    toLine = intercalate ","
    printRow r =
        [ benchName r
        , show (mean r), show (meanLB r), show (meanUB r)
        , show (stdDev r), show (stdDevLB r), show (stdDevUB r)
        ]
