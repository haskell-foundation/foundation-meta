--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll
import           Text.Read

import           Data.List (intercalate, intersperse)
import           Generation
import           BenchUtils
import           CSV

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
                r  = getRange mi
                dat = map (scale r . mean) $ csvRows csv
                val = renderVal $ JSKV
                    [ ("type", JSQStr "bar")
                    , ("data", JSKV
                        [ ("labels", JSArr $ map (JSString . benchName) $ csvRows csv)
                        , ("datasets", JSArr
                            [ JSKV
                                [ ("label", JSQStr ("in " ++ rangeToFullHName r))
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
            items <- fmap (\it -> itemSetBody (parseCSV . itemBody $ it) it)
                 <$> loadAll "benchs/*.csv"

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
