module KMonad.Klang.Types

where

import KMonad.Prelude

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

type TextMap = M.HashMap Text KExpr

data KExpr
  = KList [KExpr]    -- ^ A list of KExpr's
  | KVector [KExpr]  -- ^ A vector of KExprs
  | KSym  Text       -- ^ An abstract symbol
  | KInt  Integer    -- ^ An integer in Klang
  | KBool Bool       -- ^ A boolean in Klang
  | KNil             -- ^ The 'nil' value in Klang
  | KText Text       -- ^ A text value in Klang
  | KKeyword Text    -- ^ A keyword in Klang
  | KHashMap TextMap -- ^ A hashmap in Klang

instance Display KExpr where
  textDisplay (KInt  i)    = textDisplay i
  textDisplay (KSym  t)    = t
  textDisplay (KList ks)   = "(" <> T.intercalate " " (map textDisplay ks) <> ")"
  textDisplay (KVector ks) = "[" <> T.intercalate " " (map textDisplay ks) <> "]"
  textDisplay (KBool b)    = if b then "true" else "false"
  textDisplay (KText t)    = "\"" <> escape t  <> "\""
  textDisplay (KKeyword t) = ":" <> t
  textDisplay KNil         = "nil"

-- | Insert escape-characters in from of certain special characters
escape :: Text -> Text
escape = T.foldl' go ""
  where
    toEsc = "\\\"" :: String
    go acc c
      | c `elem` toEsc = acc `T.snoc` '\\' `T.snoc` c
      | c == '\n'      = acc `T.append` "\\n"
      | otherwise      = acc `T.snoc` c
