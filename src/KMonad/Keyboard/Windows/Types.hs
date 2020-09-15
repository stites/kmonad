{-|
Module      : KMonad.Keyboard.Windows.Types
Description : The Windows-specific representation of KeyEvent.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

NOTE: The representation here lines up with the @keyio_win.c@ module, not with
Windows in general. There is some translation happening in the c-code.

-}
module KMonad.Keyboard.Windows.Types
  ( WinKeySwitch
  , _WinKeySwitch
  )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard

-- import qualified RIO.HashMap as M



--------------------------------------------------------------------------------
-- $typ

-- | 'WinKeySwitch' is the C-representation of a a 'KeySwitch' for our Windows
-- API. They do not come with timestamps included.
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (Windows @DWORD@) signifying the *Windows keycode*.
--
data WinKeySwitch = WinKeySwitch
  { _wSwitch  :: Word8  -- ^ 0 for Press, 1 for Release
  , _wKeycode :: Word32 -- ^ Windows indentifier for 'Keycode'
  } deriving (Eq, Ord, Show)
makeLenses ''WinKeySwitch

-- | We can reference the switch stored inside WinKeySwitch
instance HasSwitch WinKeySwitch where
  switch = wSwitch . (iso (\w -> if w == 0 then Press else Release)
                          (\s -> if s == Press then 0 else 1))

-- | We can access the keycode stored in WinKeySwitch
instance HasKeycode WinKeySwitch where
  keycode = wKeycode

-- | This lets us send 'WinKeyEvent's between Haskell and C.
instance Storable WinKeySwitch where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    pure $ WinKeySwitch s c
  poke ptr a = do
    pokeByteOff ptr 0 $ a^.wSwitch
    pokeByteOff ptr 4 $ a^.wKeycode


-- mkWinKeyEvent :: WinSwitch -> WinKeycode -> WinKeyEvent
-- mkWinKeyEvent s e = WinKeyEvent (s, e)

-- | Convert a 'KeyEvent' to a 'WinKeyEvent'
--
-- NOTE: Windows keycodes are different, and I am not confident I have full
-- coverage, therefore this conversion is not total. We are going to leave this
-- error-handling in until we are sure this is covered well. Once it lines up
-- perfectly, this is essentially an Iso.
-- toWinKey :: KeyEvent -> WinKeySwitch
-- toWinKeyEvent e = WinKeyEvent (e^.switch.from _WinSwitch, e^.keycode)
  -- case toWinKeycode $ e^.keycode of
  -- Just c  -> Right $ WinKeyEvent (e^.switch.from _WinSwitch, c)
  -- Nothing -> Left . NoWinKeycodeTo $ e^.keycode

