{-|
Module      : KMonad.Keyboard.Linux.Keycode
Description : How Linux identifies its keys
Copyright   : (c) David Janssen, 2020
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

NOTE: we'd like to include this directly in the "KMonad.Keyboard.Linux.Types"
module, but that module needs to import "KMonad.Keyboard.Types", which in turn
needs to import the OS-specific 'Keycode' definition (i.e. this file).
Therefore, to avoid circular imports, this definition needs to exist in
isolation.
-}
module KMonad.Keyboard.Linux.Keycode
  ( Keycode(..)
  , _Keyname
  , keynames

  , aliasToName
  )

where

import KMonad.Prelude

import KMonad.Keyboard.Keycode
import KMonad.Util (manyToOne, _keys)

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S

--------------------------------------------------------------------------------
-- $code
--
-- We represent 'Keycode's in Linux simply as a newtype wrapper around 'Word16',
-- which is already the Linux-native representation of keycodes, saving us from
-- having to do any casting.


-- | The 'Keycode' type for linux
newtype Keycode = Keycode Word16
  deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Hashable, Show)

-- | Display any named Keycode by its name, and anything else by its number
instance Display Keycode where
  textDisplay k@(Keycode i) = "Keycode " <> case M.lookup k codeToName of
    Just t  -> t
    Nothing -> "<unnamed: " <> textDisplay i <> ">"

--------------------------------------------------------------------------------
-- $names
--
-- All the names we use to refer to 'Keycode's.

-- | The Prism encoding the naming relationship for Linux 'Keycode's
_Keyname :: Prism' Keyname Keycode
_Keyname = prism' textDisplay lookupCode
  where
    lookupCode t = case M.lookup t aliasToName of
      Just a  -> M.lookup a nameToCode
      Nothing -> M.lookup t nameToCode

-- | The set of all valid 'Keyname's
keynames :: S.HashSet Keyname
keynames = S.fromList $ aliasToName^.._keys
                     <> nameToCode^.._keys

-- | A map of names to codes
nameToCode :: M.HashMap Keyname Keycode
nameToCode = M.fromList $ linKeyNames ^.. folded . swapped

-- | A map of codes to names
codeToName :: M.HashMap Keycode Keyname
codeToName = M.fromList linKeyNames

-- | A map of aliases to their core names
aliasToName :: M.HashMap Keyname Keyname
aliasToName = M.fromList $ (aliases, linKeyAlias)
  ^.. both      -- For both tuple elements
    . folded    -- Over the elements of the list
    . manyToOne -- Reversing the (a, [b]) to [(b, a)]

--------------------------------------------------------------------------------
-- $raw
--
-- The raw-data representations of the Keyname, Keycode relationship.

-- | Raw data list of all 'Keycode' 'Keyname' correspondences
linKeyNames :: [(Keycode, Keyname)]
linKeyNames =
  zip [1..83]
    [ "esc", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" , "-" , "="
    , "bspc", "tab", "q", "w", "e", "r", "t", "y", "u" , "i", "o" , "p", "["
    , "]", "ret", "lctl", "a", "s", "d", "f" , "g", "h", "j" , "k", "l", ";"
    , "'", "`", "lsft", "\\", "z" , "x", "c", "v", "b" , "n", "m", ",", ".", "/"
    , "rsft", "kp*" , "lalt", "spc", "caps" , "f1", "f2", "f3", "f4", "f5", "f6"
    , "f7", "f8", "f9", "f10" , "num", "scrl", "kp7", "kp8", "kp9" , "kp-"
    , "kp4", "kp5", "kp6" , "kp+", "kp1", "kp2", "kp3", "kp0" , "kp."] <>
  zip [85..120]
    [ "zenk", "102d", "f11", "f12", "ro", "kata", "hira", "henk", "kahi"
    , "muhe", "kpj,", "kprt", "rctl", "kp/", "ssrq", "ralt", "feed", "home"
    , "up", "pgup", "left", "rght", "end", "down", "pgdn", "ins", "del", "macr"
    , "mute", "vold", "volu", "pwr", "kp=", "kp+-", "paus", "scl"
    ] <>
  zip [121..182]
    [ "kp,", "hang", "hanj", "yen", "lmet", "rmet", "cmps", "stop", "agn"
    , "prps", "undo", "frnt", "copy", "open", "past", "find", "cut", "help"
    , "menu", "calc", "setp", "slp", "wake", "file", "send", "delf", "xfer"
    , "prg1", "prg2", "www", "msds", "coff", "scnl", "rot", "dir", "cycl"
    , "mail", "book", "comp", "back", "fwd", "clcd", "ejct", "eccd", "nxsg"
    , "plps", "prvs", "stcd", "rec", "rew", "phon", "iso", "cfg", "hmpg", "refr"
    , "exit", "move", "edit", "scup", "scdn", "kp(", "kp)", "new", "redo" ] <>
  zip [183..194]
    [ "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21", "f22"
    , "f23", "f24"] <>
  zip [200..248]
    [ "plcd", "pscd", "prg3", "prg4", "dash", "susp", "cls", "play", "ffwd"
    , "bass", "prnt", "hp", "cmra", "soun", "ques", "emal", "chat", "srch"
    , "conn", "fina", "sprt", "shop", "alte", "cncl", "brdn", "brup", "medi"
    , "svid", "kbtg", "kbdn", "kbup", "ksnd", "repl", "fwml", "save", "docs"
    , "batt", "blue", "wlan", "uwb", "unkn", "vdnx", "vdpv", "brcc", "brau"
    , "doff", "wwan", "wmax", "rfkl", "mcmu" ]

-- | Raw data list of various 'Keyname' correspondences for specifically linuxy
-- key-names.
linKeyAlias :: [Alias]
linKeyAlias =
  [ ("hira", ["hiranaga", "hirnag"])
  , ("kata", ["katakana", "katkan"])
  , ("cmps", ["cmp"])
  ]
