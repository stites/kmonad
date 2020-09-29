{-|
Module      : KMonad.Keyboard.Keycode
Description : OS-agnostic 'Keycode' specifications
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

NOTE: Ideally we'd like to have this be part of "KMonad.Keyboard.Types", but
we'd get a circular import because:
- "KMonad.Keyboard.Types" requires OS-specific Keycode import
- OS-specific Keycode import required this module
-}

module KMonad.Keyboard.Keycode
  ( Keyname
  , RawPrint(..)
  , Alias
  , aliases
  )
where


import KMonad.Prelude

-- | 'Keyname' is just a type-alias for Text, for coding clarity.
type Keyname = Text

-- | Used to define name-to-name correspondences here
--
-- _1 : The 'core' name used by the actual lookup
-- _2 : A list of other names that should reference the same keycode
--
-- FIXME : We do not check for nor make guarantees about duplicate names.
type Alias = (Keyname, [Keyname])

-- | Class that any implementation of 'Keycode' must implement. It provides the
-- representation of a 'Keycode' when it *cannot* be found in the collection of
-- 'Keyname's for that OS.
class RawPrint a where
  rawPrint :: a -> Text

-- | A name-to-name mapping defining 'Keycode' aliases.
aliases :: [Alias]
aliases =
  [ ("ret",          ["ret", "return", "ent"])
  , ("-",            ["min", "minus"])
  , ("=",          ["eql", "equal"])
  , ("slp",          ["zzz", "sleep"])
  , ("spc",          ["space"])
  , ("pgup",         ["pageup"])
  , ("pgdn",       ["pagedown"])
  , ("ins",         ["insert"])
  , ("del",         ["delete"])
  , ("volu",         ["volup", "volumeup", "vol+"])
  , ("vold",     ["voldwn", "vol-", "voldown"])
  , ("brup",   ["bru", "br+"])
  , ("brdn", ["brdown", "brdwn", "br-"])
  , ("lalt", ["alt"])
  , ("lctl", ["ctl", "ctrl", "lctrl", "control"])
  , ("lsft", ["sft", "shft", "lshift"])
  , ("`",    ["grv"])

 
  -- TODO: CONTINUE monkeywork HERE
  -- , (KeyRightAlt,       ["ralt"])
  -- , (KeyCompose,        ["comp", "cmps", "cmp"])
  -- , (KeyLeftShift,      ["lshift", "lshft", "lsft", "shft", "sft"])
  -- , (KeyRightShift,     ["rshift", "rshft", "rsft"])
  -- , (KeyLeftCtrl,       ["lctrl", "lctl", "ctl"])
  -- , (KeyRightCtrl,      ["rctrl", "rctl"])
  -- , (KeyLeftMeta,       ["lmeta", "lmet", "met"])
  -- , (KeyRightMeta,      ["rmeta", "rmet"])
  -- , (KeyBackspace,      ["bks", "bspc"])
  -- , (KeyCapsLock,       ["caps"])
  -- , (KeyGrave,          ["grv"])
  -- , (Key102nd,          ["102d"])
  -- , (KeyForward,        ["fwd"])
  -- , (KeyScrollLock,     ["scrlck", "slck"])
  -- , (KeyPrint,          ["prnt"])
  -- , (KeyWakeUp,         ["wkup"])
  -- , (KeyLeft,           ["lft"])
  -- , (KeyRight,          ["rght"])
  -- , (KeyLeftBrace,      ["lbrc", "["])
  -- , (KeyRightBrace,     ["rbrc", "]"])
  -- , (KeySemicolon,      ["scln", ";"])
  -- , (KeyApostrophe,     ["apos", "'"])
  -- , (KeyGrave,          ["grv", "`"])
  -- , (KeyBackslash,      ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
  -- , (KeyComma,          ["comm", ","])
  -- , (KeyDot,            ["."])
  -- , (KeySlash,          ["/"])
  -- , (KeyNumLock,        ["nlck"])
  -- , (KeyKpSlash,        ["kp/"])
  -- , (KeyKpEnter,        ["kprt"])
  -- , (KeyKpPlus,         ["kp+"])
  -- , (KeyKpAsterisk,     ["kp*"])
  -- , (KeyKpMinus,        ["kp-"])
  -- , (KeyKpDot,          ["kp."])
  -- , (KeySysRq,          ["ssrq", "sys"])
  -- , (KeyIso,            ["iso"])
  -- , (KeyKbdIllumDown,   ["bldn"])
  -- , (KeyKbdIllumUp,     ["blup"])
  -- , (KeyNextSong,       ["next"])
  -- , (KeyPlayPause,      ["pp"])
  -- , (KeyPreviousSong,   ["prev"])
  ]
