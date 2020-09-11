{-# LANGUAGE DeriveAnyClass , CPP #-}
{-|
Module      : KMonad.Keyboard
Description : Basic keyboard types
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module contains or reexports all the basic, non-IO concepts related to
dealing with key codes, events, and mappings. For keyboard-related IO see
"KMonad.Keyboard.IO".

-}
module KMonad.Keyboard
  ( -- * KeyEvents and their helpers
    -- $event
    Switch(..)
  , KeyEvent
  , switch
  , keycode
  , mkKeyEvent
  , mkPress
  , mkRelease

    -- * Predicates
  , KeyPred
  , isPress
  , isRelease
  , isKeycode
  , isPressOf
  , isReleaseOf

    -- * LMaps
    -- $lmap
  , LayerTag
  , LMap

    -- * Reexports
  , Keycode
  )

where

import KMonad.Prelude

import qualified Data.LayerStack as Ls

#ifdef darwin_HOST_OS
#endif

#ifdef linux_HOST_OS
import KMonad.Keyboard.Linux.Keycode (Keycode)
#endif




--------------------------------------------------------------------------------
-- $lmap
--
-- Type aliases for specifying stacked-layer mappings


-- FIXME: This does not belong here
-- | Layers are identified by a tag that is simply a 'Text' value.
type LayerTag = Text

-- | 'LMap's are mappings from 'LayerTag'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack LayerTag Keycode a
