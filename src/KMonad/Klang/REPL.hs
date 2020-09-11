module KMonad.Klang.REPL

where

import KMonad.Prelude


import qualified RIO.Text as T
import qualified System.IO as S

import KMonad.Klang.Parser

-- | The REPL-prompt
prompt :: Text
prompt = "ϰ: "

-- | Write some text to the REPL
putT :: MonadIO m => Text -> m ()
putT = liftIO . S.putStr . T.unpack

-- | Read a line from the REPL
getLine :: MonadIO m => m Text
getLine = T.pack <$> liftIO S.getLine

-- | Put some text and follow with a new-line
putLine :: MonadIO m => Text -> m ()
putLine = liftIO . S.putStrLn . T.unpack

-- | Run the interactive REPL
runREPL :: MonadIO m => m ()
runREPL = do
  liftIO $ S.hSetBuffering S.stdout NoBuffering
  forever $ do
    putT prompt
    l   <- getLine
    case parseKExpr l of
      Left  err -> putLine err
      Right ast -> putLine .  textDisplay $ ast
