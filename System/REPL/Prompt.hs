{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- |Little helper functions for getting and putting lines.
--  
--  Names from "Data.ListLike.IO" clash with those of Prelude.
module System.REPL.Prompt (
   -- *String-generic versions of Prelude Functions
   module Data.ListLike.IO,
   putErr,
   putErrLn,
   prompt,
   -- * Prompts
   prompt',
   promptAbort,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, reverse)

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ListLike(ListLike(empty, cons, reverse))
import Data.ListLike.IO (ListLikeIO(..))
import qualified System.IO as IO
import System.REPL.Types

-- |Prints a string to stderr.
putErr :: ListLikeIO full item => full -> IO ()
putErr = hPutStr IO.stderr

-- |Prints a string, followed by a newline character, to stderr.
putErrLn :: ListLikeIO full item => full -> IO ()
putErrLn = hPutStrLn IO.stderr

-- |Prints @> @ and asks the user to input a line.
prompt :: (MonadIO m, ListLikeIO full item) => m full
prompt = prompt' ("> " :: String)

-- |Prints its first argument and, in the same line, asks the user
--  to input a line.
prompt' :: (MonadIO m, ListLikeIO full item, ListLikeIO full' item')
        => full -> m full'
prompt' s = liftIO (putStr s >> IO.hFlush IO.stdout >> getLine)

-- |The same as prompt, but aborts as soon as the user presses a given key
--  (commonly @'\ESC'@). This function temporarily tries to set the buffering mode
--  to NoBuffering via 'System.IO.hSetBuffering', which may not be supported.
--  See the documentation of 'System.IO.hSetBuffering' for details.
promptAbort :: (MonadIO m, ListLikeIO full item, ListLikeIO full' Char,
                MonadCatch m)
            => Char -> full -> m full'
promptAbort abortChar s = do
   liftIO $ putStr s
   liftIO $ IO.hFlush IO.stdout
   bufMode <- liftIO $ IO.hGetBuffering IO.stdin
   liftIO $ IO.hSetBuffering IO.stdin IO.NoBuffering
   input <- getUntil empty
            `catch` (\(e :: AskFailure) ->
                        liftIO (IO.hSetBuffering IO.stdin bufMode) >> throwM e)
   liftIO $ IO.hSetBuffering IO.stdin bufMode
   return $ reverse input
   where
      getUntil acc = do c <- liftIO $ getChar
                        if c == abortChar then throwM AbortFailure
                        else if c == '\n' then return acc
                        else                   getUntil (cons c acc)
