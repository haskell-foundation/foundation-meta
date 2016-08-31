module Generation
    ( JSVal(..)
    , renderVal
    , htmlTag
    , htmlTable
    ) where

import           Data.Monoid
import           Data.List

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

htmlTag :: String -> String -> String
htmlTag x content  = open ++ content ++ close
  where open  = "<" ++ x ++ ">"
        close = "</" ++ x ++ ">"

htmlTagAttr :: String -> [(String, String)] -> String -> String
htmlTagAttr x attrs content  = open ++ content ++ close
  where open  = "<" ++ x ++ attr ++ ">"
        close = "</" ++ x ++ ">"
        attr
            | null attrs = ""
            | otherwise  = " " ++ intercalate " " (map (\(k,v) -> k ++ "=\"" ++ v ++ "\"") attrs)

htmlTable :: [String] -> [[String]] -> String
htmlTable hdrs rows =
    htmlTagAttr "table" [("class", "table table-bordered table-hover table-condensed")]
        (  htmlTag "thead" (htmlTag "tr" (concatMap (htmlTag "th") hdrs))
        ++ "\n"
        ++ htmlTag "tbody" (mconcat $ map (htmlTag "tr" . concatMap (htmlTag "td")) rows)
        )
