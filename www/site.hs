--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll
import Debug.Trace
import           Text.Read

import           Data.List (intercalate, intersperse)

-- | Typical row out of criterion CSVs
data Row = Row
    { benchName :: String
    , mean      :: Double
    , meanLB    :: Double
    , meanUB    :: Double
    , stdDev    :: Double
    , stdDevLB  :: Double
    , stdDevUB  :: Double
    }

data CSV = CSV
    { csvHeader :: [String]
    , csvRows   :: [Row]
    }

-- | All possible js values..
data JSVal = JSKV [(String, JSVal)]
           | JSKV1 String JSVal -- just a special case
           | JSInt Int
           | JSDouble Double
           | JSString String
           | JSQStr String
           | JSArr [JSVal]
           | JSTrue
           | JSFalse

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
    toRow (Row bn m1 m2 m3 s1 s2 s3) =
        mconcat ["<tr>"
                , "<td>", bn, "</td>"
                , "<td>", prettyTime m1, "</td>"
                , "<td>", prettyTime m2, "</td>"
                , "<td>", prettyTime m3, "</td>"
                , "<td>", prettyTime s1, "</td>"
                , "<td>", prettyTime s2, "</td>"
                , "<td>", prettyTime s3, "</td>"
                , "</tr>"]
    prettyTime d
        | d < 0.000002 = show (rn d 9 0) ++ "ns"
        | d < 0.001    = show (rn d 6 0) ++ "us"
        | d < 1        = show (rn d 3 0) ++ "ms"
        | otherwise    = show (rn d 0 1) ++ "s"
    rn :: Double -> Int -> Int -> Double
    rn d prec n = fromIntegral (round (d * (10 ^ (n+prec))) :: Int) / (10^n)

-- | the best range associated with a (Double) time value in seconds
data Range = RSeconds | RMilliSeconds | RMicroSeconds | RNanoSeconds

doubleToRange :: Double -> Range
doubleToRange d
    | d < 0.000002 = RNanoSeconds
    | d < 0.002    = RMicroSeconds
    | d < 1.5      = RMilliSeconds
    | otherwise    = RSeconds

rangeToHName :: Range -> String
rangeToHName RSeconds      = "s"
rangeToHName RMilliSeconds = "ms"
rangeToHName RMicroSeconds = "us"
rangeToHName RNanoSeconds  = "ns"

scale :: Range -> Double -> Double
scale r d = fromIntegral (round (d * (10 ^ (n+prec))) :: Int) / (10^n)
  where
    (prec, n) = case r of
                    RSeconds      -> (0,1)
                    RMilliSeconds -> (3,0)
                    RMicroSeconds -> (6,0)
                    RNanoSeconds  -> (9,0)

-- render a JS value as a string ready for inclusion somewhere
renderVal :: JSVal -> String
renderVal (JSKV [])    = "{}"
renderVal (JSKV l)     = "{ " ++ intercalate ", " (map toField l) ++ " }" where toField (k,v)  = k ++ ": " ++ renderVal v
renderVal (JSKV1 k v)  = "{ " ++ k ++ ": " ++ renderVal v ++ " }"
renderVal (JSInt i)    = show i
renderVal (JSDouble d) = show d
renderVal (JSString s) = show s
renderVal (JSQStr s)   = "'" ++ s ++ "'"
renderVal (JSArr l)    = "[" ++ intercalate ", " (map renderVal l) ++ "]"
renderVal JSTrue       = "true"
renderVal JSFalse      = "false"

-- obviously not a fully compliant csv parser. if it contains quoted string it will explode.
contentToCSV :: Item String -> Item CSV
contentToCSV it = itemSetBody (toCSV $ itemBody it) it
  where
        toCSV content =
            case map (wordsWhen (== ',')) $ lines content of
                hdr:rows -> CSV hdr (map toRow rows)
        toRow (n:m:ml:mu:s:sl:su:[]) =
            Row n (rDouble m) (rDouble ml) (rDouble mu) (rDouble s) (rDouble sl) (rDouble su)
          where rDouble s =
                    case readMaybe s of
                        Nothing -> error ("couldn't read double : " ++ show s)
                        Just d  -> d
        toRow l = error ("wrong format of row: " ++ show l)

        wordsWhen :: (Char -> Bool) -> String -> [String]
        wordsWhen p s = case dropWhile p s of
            "" -> []
            s' -> w : wordsWhen p s''
                where (w, s'') = break p s'

canvasNameFromItem :: Item CSV -> String
canvasNameFromItem it = toHtmlId $ show (itemIdentifier it)
  where toHtmlId = map toSane
        toSane :: Char -> Char
        toSane c
            | c `elem` badChars = '_'
            | otherwise         = c
        badChars :: String
        badChars = "./"

backColor = JSArr $ map JSQStr
    ["rgba(255, 99, 132, 0.2)",
    "rgba(54, 162, 235, 0.2)",
    "rgba(255, 206, 86, 0.2)",
    "rgba(75, 192, 192, 0.2)",
    "rgba(153, 102, 255, 0.2)",
    "rgba(255, 159, 64, 0.2)"
    ]
borderColor = JSArr $ map JSQStr
    [ "rgba(255,99,132,1)",
    "rgba(54, 162, 235, 1)",
    "rgba(255, 206, 86, 1)",
    "rgba(75, 192, 192, 1)",
    "rgba(153, 102, 255, 1)",
    "rgba(255, 159, 64, 1)"
    ]


csvToJs :: Item CSV -> String
csvToJs it = toJs $ itemBody it
  where toJs csv =
            let mi = minimum $ map mean $ csvRows csv
                r  = doubleToRange mi
                dat = map (scale r . mean) $ csvRows csv
                val = renderVal $ JSKV
                    [ ("type", JSQStr "bar")
                    , ("data", JSKV
                        [ ("labels", JSArr $ map (JSString . benchName) $ csvRows csv)
                        , ("datasets", JSArr
                            [ JSKV
                                [ ("label", JSQStr ("in " ++ rangeToHName r))
                                , ("data", JSArr $ map JSDouble dat)
                                , ("backgroundColor", backColor)
                                , ("borderColor", borderColor)
                                , ("borderWidth", JSInt 1)
                                ]
                            ])
                        ])
                    , ("options", JSKV1 "scales" $ JSKV1 "yAxes" $ JSArr [ JSKV1 "ticks" $ JSKV1 "beginAtZero" JSTrue ]
                      )
                    ]
             in "new Chart(document.getElementById(\"" ++ canvasNameFromItem it ++ "\"), " ++ val ++ " );"
--  var mybarChartLoc = new Chart(document.getElementById("barChartLoc"), myChartD);

-- like applyTemplateList, but also change context at each item
applyTemplateListItem :: String -> Template -> (Item a -> Context a) -> [Item a] -> Compiler String
applyTemplateListItem delimiter tpl getContext items = do
    items' <- mapM (\i -> applyTemplate tpl (getContext i) i) items
    return $ concat $ intersperse delimiter $ map itemBody items'

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "vendor/**" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["features.md", "documentation.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/md.html" defaultContext
            >>= applyPage (PageCtx Nothing "")
            >>= relativizeUrls

{-
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= applyPage (PageCtx Nothing "")
            >>= relativizeUrls
            -}

    match "benchs/*.csv" $ compile (getResourceBody)

    create ["bench.html"] $ do
        route idRoute
        compile $ do
            items <- fmap contentToCSV <$> loadAll "benchs/*.csv"

            let benchCtx it = mconcat
                    [ constField "name" ("name " ++ show (itemIdentifier it))
                    , constField "canvas_name" (canvasNameFromItem it)
                    , constField "canvas_height" "400"
                    , constField "canvas_width" "400"
                    , constField "csvtable" (csvToHtml $ itemBody it)
                    ]
            -- add placeholders and create snippet for the end

            itemTpl <- loadBody "templates/csv-to-js.html"
            list <- applyTemplateListItem "" itemTpl benchCtx items

            let fillCanvas = fmap csvToJs items :: [String]

            let footerScript = intercalate "\n" $
                       [ "<script>"
                       , "$(function() {" ]
                    ++ fillCanvas
                    ++ [ "});"
                       , "</script>"]

            makeItem list
                >>= loadAndApplyTemplate "templates/benchs.html" defaultContext
                >>= applyPage (PageCtx (Just "Foundation: Benchmarks") footerScript)
                >>= relativizeUrls

{-
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= applyPage (PageCtx (Just "Archives") "")
                >>= relativizeUrls
                -}

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/main.html" indexCtx
                >>= applyPage (PageCtx (Just "Home") "")
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

data PageCtx = PageCtx
    { pageTitle        :: Maybe String
    , pageFooterScript :: String
    }

applyPage c = loadAndApplyTemplate "templates/default.html" ctx
  where
    ctx = mconcat
        [ constField "footerjs" (pageFooterScript c)
        , dateField "date" "%B %e, %Y"
        , optFields
        , defaultContext
        ]
    optFields :: Context a
    optFields = mconcat $ concat
        [ maybe [] (\x -> [constField "title" x]) (pageTitle c)
        ]

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
