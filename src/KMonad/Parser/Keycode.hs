module KMonad.Parser.Keycode

where

import KMonad.Prelude

import KMonad.Keyboard

import qualified RIO.HashMap as M

-- | A map of keycode names to 'Keycode's.
--
-- This is combination of the OS-specific keycode names defined in their respective
keyNames :: M.HashMap Text Keycode
keyNames = undefined



-- aliases :: [(Text, [Text])]
-- aliases =
--   [ ("esc",  ["escape"])
--   , ("-",    ["minus", "min"])
--   , ("=",    ["equal", "eq", "eql"])
--   , ("slp",  ["zzz"])
--   , ("spc",  ["space"])
--   , ("pgup", ["pageup"])
--   , ("pgdn", ["pagedn", "pagedown"])
--   ]
