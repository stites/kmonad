{-|
Module      : KMonad.Hooks
Description : Implementation of hook logic
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Hooks are locations in KMonad where you can 'hook' functions into the stream of
events. There are currently 2 different locations we support:

- input hook: triggers after an event is past the sluice, but before its handed
  to the keymap.
- output hook: triggers just before an event is written to the OS.

How a hook triggers can be customized. There are 2 settings that can be set.

1. A hook can be given a timeout. After a certain amount of time has passed, a
timeout event will be written to the hook. If the hook is no longer registered
by the time this happens, it is simply ignored. If the hook is still active, its
timeout event is triggered and the hook is forcibly unregistered.

2. A hook can be either persistent or one-shot. A one-shot hook triggers on the
next event and is automatically unregistered. A persistent hook will be called
on all events until it explicitly unregisters itself. A persistent hook with a
timeout will also be unregistered if the timeout triggers.

-}

module KMonad.Hooks
  -- ()
where

import KMonad.Prelude hiding (timeout)

import Data.Unique

import KMonad.Keyboard
import KMonad.Util


-- | Boolean isomorph signalling wether an event should be caught or not
data Catch = Catch | NoCatch deriving (Show, Eq)

-- | Semigroup instance for Catch, behaves like 'Any', where Catch is True
instance Semigroup Catch where
  NoCatch <> NoCatch = NoCatch
  _       <> _       = Catch

-- | Default is to not catch
instance Monoid Catch where
  mempty = NoCatch

-- | The packet used to trigger a KeyFun
data Trigger = Trigger
  { _elapsed :: Milliseconds -- ^ Time elapsed since hook was registered
  , _event   :: KeyEvent     -- ^ The key event triggering this call
  }
makeClassy ''Trigger

-- | ADT signalling where to install a hook
data HookLocation
  = InputHook  -- ^ Install the hook immediately after receiving a 'KeyEvent'
  | OutputHook -- ^ Install the hook just before emitting a 'KeyEvent'
  deriving (Eq, Show)

-- | ADT signalling basic hook behavior type
data HookType
  = Persist -- ^ A hook that keeps existing until explicitely unregistered
  | OneShot -- ^ A hook that triggers once and is automatically removed
  deriving (Eq, Show)

-- | A 'Timeout' value describes how long to wait and what to do upon timeout
data Timeout m = Timeout
  { _delay  :: Milliseconds -- ^ Delay before timeout action is triggered
  , _action :: m ()         -- ^ Action to perform upon timeout
  }
makeClassy ''Timeout

-- | Unique identifier for a hook
newtype HookId = HookId Unique
  deriving (Eq, Ord, Hashable)

-- | The content for 1 key hook
data Hook m = Hook
  { _hkTimeout :: Maybe (Timeout m)  -- ^ Optional timeout machinery
  , _hkType    :: HookType           -- ^ Whether this hook should persist over time
  , _hkId      :: HookId             -- ^ Unique identifier for this hook
  , _keyH      :: Trigger -> m Catch -- ^ The function to call on 'KeyEvent's
  }
makeClassy ''Hook


-- | A function that generates 'HookId's, used to implement MonadK
newHookId :: MonadIO m => m HookId
newHookId = HookId <$> liftIO newUnique
