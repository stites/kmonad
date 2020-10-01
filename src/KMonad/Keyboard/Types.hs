{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass, CPP #-}
{-|
Module      : KMonad.Keyboard.Types
Description : Basic keyboard types
Copyright   : (c) David Janssen, 2020
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The following module contains all the basic, OS-agnostic types that we use to
reason about keyboards, their elements, and their events. Note that some of the
terminology can be ambiguous, so for clarity's sake this is how we use
terminology in KMonad:

A keyboard is a collection of different entities that can exist as either
pressed or released.

When we say *key* we are using this to refer to the unique identifiability of an
element (by its 'Keycode').

When we say *button* we are using this to refer to the fact that the elements of
a keyboard have 2 states, and that changing between these states causes actions
to occur.

Buttons are bound to keys, and when a state-change associated with a particular
key is detected ('KeyEvent'), that key's button is looked up, and it's state is
similarly changed.

-}
module KMonad.Keyboard.Types
  ( -- * Keycode
    -- $code
    Keycode
  , HasKeycode(..)
  , matchCode
  , kc
  , keynames
  , _RawName

    -- * Switch
    -- $switch
  , Switch(..)
  , HasSwitch(..)
  , matchSwitch
  , isPress, isRelease

    -- * KeySwitch
    -- $keyswitch
  , KeySwitch
  , HasKeySwitch(..)
  , getKeySwitch
  , pressOf, releaseOf
  , isPressOf, isReleaseOf
  , matchKeySwitch
 
    -- * KeyEvent
    -- $event
  , KeyEvent
  , KeyPred
  , mkKeyEvent
  )
where

import KMonad.Prelude

import KMonad.Keyboard.Keycode
import KMonad.Util

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S
import qualified RIO.Text    as T

#ifdef linux_HOST_OS
import KMonad.Keyboard.Linux.Keycode
#endif

#ifdef mingw32_HOST_OS
import KMonad.Keyboard.Windows.Keycode
#endif


--------------------------------------------------------------------------------
-- $code
--
-- 'Keycode's are what an OS uses to identify which key is being pressed. In the
-- following section we define the OS-agnostic 'Keycode' support in KMonad. This
-- wraps around the OS-specific 'Keycode' definitions imported above.

-- | A class describing how to access some value's 'Keycode'.
class HasKeycode a where
  keycode :: Lens' a Keycode

-- | Hooks the class specific definition of 'Keycode' into 'HasKeycode'
instance HasKeycode Keycode where
  keycode = id

-- | Return whether the keycodes match between 2 values.
matchCode :: (HasKeycode a, HasKeycode b) => a -> b -> Bool
matchCode a b = a^.keycode == b^.keycode

-- | Lookup a 'Keycode' by its name and throw an error if it does not exist.
--
-- NOTE: This is mainly for internal use, so you can reference the 'Keycode'
-- for, e.g., the letter `a` through `kc a` in an OS-independent manner
kc :: Text -> Keycode
kc c = case c ^? _Keyname of
  Nothing -> error $ "Lookup failed during parsing: " <> T.unpack c
  Just a  -> a


--------------------------------------------------------------------------------
-- $names
--
-- All the names we use to refer to 'Keycode's.

-- | A map of raw, unaliased names to codes
nameToCode :: M.HashMap Keyname Keycode
nameToCode = M.fromList $ osKeynames ^.. folded . swapped

-- | A map of keycodes to their core name (no aliases)
codeToName :: M.HashMap Keycode Keyname
codeToName = M.fromList osKeynames

-- | A map of aliases to their core names
aliasToName :: M.HashMap Keyname Keyname
aliasToName = M.fromList $ (aliases, osAliases)
  ^.. both      -- For both tuple elements
    . folded    -- Over the elements of the list
    . manyToOne -- Reversing the (a, [b]) to [(b, a)]

-- | The Prism encoding the naming relationship for Linux 'Keycode's
_Keyname :: Prism' Keyname Keycode
_Keyname = prism' textDisplay lookupCode
  where
    lookupCode t = case M.lookup t aliasToName of
      Just a  -> M.lookup a nameToCode
      Nothing -> M.lookup t nameToCode

-- | Display any named Keycode by its name, and anything else by a decorated
--   version of its raw representation.
instance Display Keycode where
  textDisplay k = "Keycode " <> case M.lookup k codeToName of
    Just t  -> t
    Nothing -> "<unnamed: " <> _RawName # k <> ">"
   
-- | The set of all valid 'Keyname's
keynames :: S.HashSet Keyname
keynames = S.fromList $ aliasToName^.._keys <> nameToCode^.._keys

--------------------------------------------------------------------------------
-- $switch
--
-- 'Switch' describes a state-transition for a 2-state system, with the
-- additional semantics of some sort of /enabled/ vs. /disabled/ context.

-- | An ADT describing all the state-changes a button or key can undergo.
data Switch
  = Press   -- ^ Change from disabled to enabled
  | Release -- ^ Change from enabled to disabled
  deriving (Eq, Ord, Enum, Show, Generic, Hashable)

-- | A class describing how to access some value's 'Switch'
class HasSwitch a where
  switch :: Lens' a Switch

-- | Hooks the base 'Switch' type into 'HasSwitch'
instance HasSwitch Switch where switch = id

-- | How to pretty-print a 'Switch'
instance Display Switch where
  textDisplay = tshow

-- | Return whether two values match on their 'switch' value
matchSwitch :: (HasSwitch a, HasSwitch b) => a -> b -> Bool
matchSwitch a b = a^.switch == b^.switch

-- | Return whether a value contained a 'Press'
-- isPress :: HasSwitch a => a -> Bool
isPress :: HasSwitch a => a -> Bool
isPress = (Press ==) . view switch

-- | Return whether a value contained a 'Release'
isRelease :: HasSwitch a => a -> Bool
isRelease = (Release ==) . view switch

--------------------------------------------------------------------------------
-- $keyswitch
--
-- A 'KeySwitch' is a detected state-change for some key, identified by its
-- 'Keycode'.

-- | The 'KeySwitch' record, containing a 'Switch' and an identifying 'Keycode'
data KeySwitch = KeySwitch
  { _ksSwitch :: !Switch  -- ^ wether a 'press' or 'release' occurred
  , _ksCode   :: !Keycode -- ^ the identity of the key which registered the event
  } deriving (Eq, Show)
makeLenses ''KeySwitch

class HasKeySwitch a where
  keySwitch :: Lens' a KeySwitch

-- | Class describing all the necessary information to function as a 'KeySwitch'
type CanKeySwitch a = (HasKeycode a, HasSwitch a)

-- | Extract a 'KeySwitch' from something can 'CanKeySwitch'
getKeySwitch :: CanKeySwitch a => a -> KeySwitch
getKeySwitch a = KeySwitch (a^.switch) (a^.keycode)

instance HasKeySwitch KeySwitch where keySwitch = id       -- ^ Hook into HasKeySwitch
instance HasSwitch    KeySwitch where switch    = ksSwitch -- ^ Hook into HasSwitch
instance HasKeycode   KeySwitch where keycode   = ksCode   -- ^ Hook into HasKeycode

-- | How to pretty-print a 'KeySwitch'
instance Display KeySwitch where
  textDisplay e = tshow (e^.switch) <> " " <> textDisplay (e^.keycode)

-- | Create a new 'Press' 'KeySwitch'
pressOf :: Keycode -> KeySwitch
pressOf = KeySwitch Press

-- | Create a new 'Release' 'KeySwitch'
releaseOf :: Keycode -> KeySwitch
releaseOf = KeySwitch Release

-- | Return whether two values match on both 'Keycode' and 'Switch'
matchKeySwitch :: (CanKeySwitch a, CanKeySwitch b)
  => a -> b -> Bool
matchKeySwitch a b = matchCode a b && matchSwitch a b

-- | Return whether a value matches against the press of a provided 'Keycode'
isPressOf :: CanKeySwitch a => Keycode -> a -> Bool
isPressOf c = matchKeySwitch (pressOf c)

-- | Return whether a value matches against the release of a provided 'Keycode'
isReleaseOf :: CanKeySwitch a => Keycode -> a -> Bool
isReleaseOf c = matchKeySwitch (releaseOf c)


--------------------------------------------------------------------------------
-- $keyevent
--
-- A 'KeyEvent' is a 'KeySwitch' at some point in time.

-- | A 'KeyEvent' indicates the changing of a switch registered to a particular
-- 'Keycode' as some 'UTCTime'.
data KeyEvent = KeyEvent
  { _keKeySwitch :: !KeySwitch  -- ^ Details of the state-change
  , _keTime      :: !UTCTime    -- ^ When the event occured
  }
makeLenses ''KeyEvent

instance HasKeySwitch KeyEvent where keySwitch = keKeySwitch
instance HasSwitch    KeyEvent where switch    = keySwitch.switch
instance HasKeycode   KeyEvent where keycode   = keySwitch.keycode
instance HasTime      KeyEvent where time      = keTime

-- | How to pretty-print a 'KeyEvent'
instance Display KeyEvent where
  textDisplay = textDisplay . view keySwitch

-- | We use on predicates on 'KeyEvent' enough to warrant a type-alias
type KeyPred = KeyEvent -> Bool

-- | Create a new 'KeyEvent'
mkKeyEvent :: Switch -> Keycode -> UTCTime -> KeyEvent
mkKeyEvent s c = KeyEvent (KeySwitch s c)

