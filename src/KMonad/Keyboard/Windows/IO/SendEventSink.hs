{-|
Module      : KMonad.Keyboard.Windows.IO.SendEventSink
Description : Using Windows' send_event functionality to inject KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This uses @sendKey@ from the @keyio_win.c@ to send keys to Windows. This itself
then uses the Windows 'SendInput' system call.

-}
module KMonad.Keyboard.Windows.IO.SendEventSink
  ( sendEventKeySink
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.Windows.Types
import KMonad.Util

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $ffi
--
-- The C-function wrappings for this module

-- | The C-function that sends an event to Windows through @SendKey@
foreign import ccall "sendKey"
  c_sendKey :: Ptr WindowsEvent -> IO ()

sendKey :: Ptr WindowsEvent -> WindowsEvent -> IO ()
sendKey ptr e = poke ptr e >> c_sendKey ptr
 
--------------------------------------------------------------------------------
-- $rep
--
-- In Windows the keystream is encoded as a series of press events. Every press
-- event triggers a key-repeat. KMonad functions only on alternating
-- Press/Release events, so we have to manually add the key-repeat behavior back
-- in.

-- | The 'Repeater' record
data Repeater a = Repeater
  { _krDelay :: Milliseconds
  , _krRate  :: Milliseconds
  , _krFun   :: a -> IO ()
  , _krState :: MVar (M.HashMap a (Async ()))
  }
makeClassy ''Repeater

-- | Initialize the 'KeyRepeat' environment
mkRepeater :: Hashable a
  => Milliseconds       -- ^ Delay until we start repeating
  -> Milliseconds       -- ^ Time between repeats
  -> (a -> IO ())       -- ^ How to decide what to do
  -> RIO e (Repeater a) -- ^ The environment
mkRepeater d r f = Repeater d r f <$> newMVar M.empty

repStart :: HasRepeater e a => a -> RIO e ()
repStart = do
  undefined

repStop :: HasRepeater e a => a -> RIO e ()
repStop = do
  undefined

--------------------------------------------------------------------------------


-- | Return a 'KeySink' using Window's @sendEvent@ functionality.
sendEventKeySink :: HasLogFunc e
  => Milliseconds -- ^ Delay before a key starts repeating
  -> Milliseconds -- ^ Time between key-repeats
  -> RIO e (Acquire KeySink)
sendEventKeySink d r = mkKeySink (skOpen d r) skClose skSend


-- | The SKSink environment
data SKSink = SKSink
  { _buffer :: Ptr WindowsEvent
  , _keyRep :: Repeater Keycode -- ^ Repeat settings
  }
makeClassy ''SKSink

-- instance HasRepeater SKSink Keycode where repeater = keyRep


-- | Create the 'SKSink' environment
skOpen :: HasLogFunc e => Milliseconds -> Milliseconds -> RIO e SKSink
skOpen d r = do
  logInfo "Initializing Windows key sink"
  ptr <- liftIO $ mallocBytes (sizeOf (undefined :: WindowsEvent))
  rep <- mkRepeater d r (sendKey ptr)
  pure $ SKSink ptr rep

-- | Close the 'SKSink' environment
skClose :: HasLogFunc e => SKSink -> RIO e ()
skClose sk = do
  logInfo "Closing Windows key sink"
  liftIO . free $ sk^.buffer

repPress :: Keycode -> IO ()
repPress = undefined

-- | Write an event to the pointer and prompt windows to inject it
skSend :: HasLogFunc e => SKSink -> KeyEvent -> RIO e ()
skSend sk e = go $ toWindowsEvent e
  where go e' = liftIO $ do
          poke (sk^.buffer) e'
          sendKey $ sk^.buffer
