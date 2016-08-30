module Generation
    ( JSVal(..)
    , renderVal
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
