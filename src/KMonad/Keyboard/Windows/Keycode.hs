module KMonad.Keyboard.Windows.Keycode
  (Keycode, keycodeNames)
where

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $code
--
-- We represent 'Keycode's in Windows simply as a newtype wrapper around
-- 'Word32', which is already the Windows-native representation of keycodes,
-- saving us from having to do any casting.

-- | The 'Keycode' type for linux
newtype Keycode = Keycode Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Hashable, Show, Display)

-- type WinKeycode = Word32 -- ^ Type alias for the windows encoded keycode
-- | Lookup the corresponding 'Keycode' for this 'WinKeycode'
-- fromWinKeycode :: WinKeycode -> Maybe Keycode
-- fromWinKeycode = flip M.lookup kcMap

-- | Lookup the correspondig 'WinKeycode' for this 'Keycode'
-- toWinKeycode :: Keycode -> Maybe WinKeycode
-- toWinKeycode = flip M.lookup revMap
--   where revMap = M.fromList $ (M.toList kcMap) ^.. folded . swapped

-- FIXME: There are loads of missing correspondences, mostly for rare-keys. How
-- do these line up? Ideally this mapping would be total.
keycodeNames :: [(Keycode, Text)]
keycodeNames = map (first Keycode)
  [
  --  (0x00, "???")     -- Not documented, but happens often. Why??
    (0x08, "bspc")
  , (0x09, "tab")
  , (0x0C, "kp5") -- VK_CLEAR: NumPad 5 when numlock is not engaged
  , (0x0D, "ret")
  , (0x10, "lsft")   -- No 'sidedness'??
  , (0x11, "lctl")    -- No 'sidedness'??
  , (0x12, "lalt")     -- No 'sidedness'??
  , (0x13, "paus")
  , (0x14, "caps")
  , (0x15, "kata")    -- Also: KeyHangul
  -- , (0x17, ???)            -- Defined as VK_JUNJjj
  -- , (0x18, ???)            -- Defined as VK_HANJA
  -- , (0x19, ???)            -- Defined as VK_KANJI
  , (0x1B, "esc")
  -- , (0x1C, ???)            -- Defined as VK_CONVERT
  -- , (0x1D, ???)            -- Defined as VK_NONCONVERT
  -- , (0x1E, ???)            -- Defined as VK_ACCEPT
  -- , (0x1F, ???)            -- Defined as VK_MODECHANGE
  , (0x20, "spc")
  , (0x21, "pgup")
  , (0x22, "pgdn")
  , (0x23, "end")
  , (0x24, "home")
  , (0x25, "left")
  , (0x26, "up")
  , (0x27, "rght")
  , (0x28, "down")
  -- , (0x29, ???)            -- Defined as VK_SELECT
  , (0x2A, "prnt")
  -- , (0x2B, ???)            -- Defined as VK_EXECUTE
  , (0x2C, "ssrq")          -- Defined as VK_PRINT_SCREEN
  , (0x2D, "ins")
  , (0x2E, "del")
  , (0x2F, "help")
  , (0x30, "0")
  , (0x31, "1")
  , (0x32, "2")
  , (0x33, "3")
  , (0x34, "4")
  , (0x35, "5")
  , (0x36, "6")
  , (0x37, "7")
  , (0x38, "8")
  , (0x39, "9")
  , (0x41, "a")
  , (0x42, "b")
  , (0x43, "c")
  , (0x44, "d")
  , (0x45, "e")
  , (0x46, "f")
  , (0x47, "g")
  , (0x48, "h")
  , (0x49, "i")
  , (0x4A, "j")
  , (0x4B, "k")
  , (0x4C, "l")
  , (0x4D, "m")
  , (0x4E, "n")
  , (0x4F, "o")
  , (0x50, "p")
  , (0x51, "q")
  , (0x52, "r")
  , (0x53, "s")
  , (0x54, "t")
  , (0x55, "u")
  , (0x56, "v")
  , (0x57, "w")
  , (0x58, "x")
  , (0x59, "y")
  , (0x5A, "z")
  , (0x5B, "lmet")             -- Defined as Left Windows key (Natural Keyboard)
  , (0x5C, "rmet")             -- Defined as Right Windows key (Natural Keyboard)
  , (0x5D, "cps")             -- Defined as Applications key (Natural Keyboard)
  , (0x5F, "slp")
  , (0x60, "kp0")
  , (0x61, "kp1")
  , (0x62, "kp2")
  , (0x63, "kp3")
  , (0x64, "kp4")
  , (0x65, "kp5")
  , (0x66, "kp6")
  , (0x67, "kp7")
  , (0x68, "kp8")
  , (0x69, "kp9")
  , (0x6A, "kp*")
  , (0x6B, "kp+")
  -- , (0x6C, KeyKpDot)        -- Defined as VK_SEPARATOR
  , (0x6D, "kp-")
  , (0x6E, "kp.")
  , (0x6F, "kp/")
  , (0x70, "f1")
  , (0x71, "f2")
  , (0x72, "f3")
  , (0x73, "f4")
  , (0x74, "f5")
  , (0x75, "f6")
  , (0x76, "f7")
  , (0x77, "f8")
  , (0x78, "f9")
  , (0x79, "f10")
  , (0x7A, "f11")
  , (0x7B, "f12")
  , (0x7C, "f13")
  , (0x7D, "f14")
  , (0x7E, "f15")
  , (0x7F, "f16")
  , (0x80, "f17")
  , (0x81, "f19")
  , (0x83, "f20")
  , (0x84, "f21")
  , (0x85, "f22")
  , (0x86, "f23")
  , (0x87, "f24")
  , (0x90, "nlck")
  , (0x91, "slck")
  , (0xA0, "lsft")
  , (0xA1, "rsft")
  , (0xA2, "lctl")
  , (0xA3, "rctl")
  , (0xA4, "lalt")
  , (0xA5, "ralt")
  -- , (0xA6, ???)             -- Defined as VK_BROWSER_BACK
  -- , (0xA7, ???)             -- Defined as VK_BROWSER_FORWARD
  -- , (0xA8, ???)             -- Defined as VK_BROWSER_REFRESH
  -- , (0xA9, ???)             -- Defined as VK_BROWSER_STOP
  -- , (0xAA, ???)             -- Defined as VK_BROWSER_SEARCH
  -- , (0xAB, ???)             -- Defined as VK_BROWSER_FAVORITES
  -- , (0xAC, ???)             -- Defined as VK_BROWSER_HOME
  , (0xAD, "mute")
  , (0xAE, "vold")
  , (0xAF, "volu")
  , (0xB0, "next")
  , (0xB1, "prev")
  , (0xB2, "stop")
  -- , (0xB3, KeyPlayPause)
  , (0xB4, "mail")
  , (0xB5, "med")
  -- , (0xB6, ???)             -- Defined as VK_LAUNCH_APP1
  -- , (0xB7, ???)             -- Defined as VK_LAUNCH_APP2
  , (0xBA, ";")    -- Defined as VK_OEM_1
  , (0xBB, "=")        -- Defined as VK_OEM_PLUS
  , (0xBC, ",")        -- Defined as VK_OEM_COMMA
  , (0xBD, "-")        -- Defined as VK_OEM_MINUS
  , (0xBE, ".")          -- Defined as VK_OEM_PERIOD
  , (0xBF, "/")        -- Defined as VK_OEM_2
  , (0xC0, "`")        -- Defined as VK_OEM_3
  , (0xDB, "[")    -- Defined as VK_OEM_4
  , (0xDC, "\\")    -- Defined as VK_OEM_5
  , (0xDD, "]")   -- Defined as VK_OEM_6
  , (0xDE, "'")   -- Defined as VK_OEM_7
  -- , (0xDF, ???)             -- Defined ask VK_OEM_8
  -- , (0xE1, ???)             -- Defined as `OEM specific`
  , (0xE2, "102d")
  -- , (0xE3, ???)             -- Defined as `OEM specific`
  -- , (0xE4, ???)             -- Defined as `OEM specific`
  -- , (0xE5, ???)             -- Defined as OEM PROCESS key
  -- , (0xE6, ???)             -- Defined as `OEM specific`
  -- , (0xE7, ???)             -- Defined as VK_PACKET
  -- , (0xE9, ???)             -- Defined as `OEM specific`
  -- , (0xEA, ???)             -- Defined as `OEM specific`
  -- , (0xEB, ???)             -- Defined as `OEM specific`
  -- , (0xEC, ???)             -- Defined as `OEM specific`
  -- , (0xED, ???)             -- Defined as `OEM specific`
  -- , (0xEE, ???)             -- Defined as `OEM specific`
  -- , (0xEF, ???)             -- Defined as `OEM specific`
  -- , (0xF0, ???)             -- Defined as `OEM specific`
  -- , (0xF1, ???)             -- Defined as `OEM specific`
  -- , (0xF2, ???)             -- Defined as `OEM specific`
  -- , (0xF3, ???)             -- Defined as `OEM specific`
  -- , (0xF4, ???)             -- Defined as `OEM specific`
  -- , (0xF5, ???)             -- Defined as `OEM specific`
  -- , (0xF6, ???)             -- Defined as VK_ATTN
  -- , (0xF7, ???)             -- Defined as VK_CRSEL
  -- , (0xF8, ???)             -- Defined as VK_EXSEL
  -- , (0xF9, ???)             -- Defined as VK_EREOF
  , (0xFA, "play")
  -- , (0xFB, ???)             -- Defined as VK_ZOOM
  -- , (0xFC, ???)             -- Defined as VK_NONAME
  -- , (0xFD, ???)             -- Defined as VK_PA1
  -- , (0xFE, ???)             -- Defined as VK_CLEAR
  ]
