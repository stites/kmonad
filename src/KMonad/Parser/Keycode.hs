module KMonad.Parser.Keycode

where

import KMonad.Prelude


aliases :: [(Text, [Text])]
aliases =
  [ ("esc",  ["escape"])
  , ("-",    ["minus", "min"])
  , ("=",    ["equal", "eq", "eql"])
  , ("slp",  ["zzz"])
  , ("spc",  ["space"])
  , ("pgup", ["pageup"])
  , ("pgdn", ["pagedn", "pagedown"])
  ]
