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
  ( Keycode(..) )
where

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $code
--
-- We represent 'Keycode's in Linux simply as a newtype wrapper around 'Word16',
-- which is already the Linux-native representation of keycodes, saving us from
-- having to do any casting.

-- | The 'Keycode' type for linux
newtype Keycode = Keycode Word16
  deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Hashable, Show, Display)
