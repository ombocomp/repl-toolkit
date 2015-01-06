{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |Functions to expedite the building of REPLs.
module System.REPL (
   -- *String-generic versions of Prelude Functions
   module Data.ListLike.IO,
   putErr,
   putErrLn,
   prompt,
   -- * Prompts
   prompt',
   promptAbort,
   -- *Feture-rich reading of user-input
   -- |These functions automate parsing and validating command-line
   --  input via the 'Asker' type.
   PromptMsg,
   TypeErrorMsg,
   PredicateErrorMsg,
   Predicate,
   Parser,
   Asker(..),
   AskFailure(..),
   askerP,
   typeAskerP,
   maybeAskerP,
   -- **Asking based on 'Read'
   -- |These askers use 'Text.Read.readMaybe' as their parser.
   --
   --  It is possible to ask for Strings, but then quotes will be required
   --  around them (per their Read-instance). To get the user's
   --  input as-is, use the 'Verbatim' type.
   readParser,
   asker,
   typeAsker,
   predAsker,
   maybeAsker,
   Verbatim(..),
   -- **Running askers
   -- |Since the parsing depends on the Read-instance, the expected result type
   --  must be explicitly given. E.g.:
   --
   -- @
   --   intAsker :: Asker IO Int
   --   intAsker = typeAsker "> " "Expected Int!"
   -- @
   --
   -- or, for polymorphic askers,
   --
   -- @
   --   genericAsk :: Read a => Asker IO a
   --   genericAsk = typeAsker "> " "Couldn't parse value!"
   --   ...
   --   do (x :: Int) <- genericAsk
   --      (y :: Int) <- genericAsk
   --      putStrLn $ "The sum is: " ++ show (x+y)
   -- @
   ask,
   ask',
   askEither,
   untilValid,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, reverse)

import Control.Arrow (right, (|||))
import Control.Exception
import Control.Monad.Except
import Data.Char (isSpace)
import Data.Functor.Monadic
import Data.ListLike(ListLike(empty, cons, reverse))
import Data.ListLike.IO (ListLikeIO(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Typeable
import qualified System.IO as IO
import Text.Read (readMaybe)

-- Stdio
-------------------------------------------------------------------------------

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
promptAbort :: (MonadIO m, ListLikeIO full item, ListLikeIO full' Char)
            => Char -> full -> m (Maybe full')
promptAbort abortChar s = liftIO (do putStr s
                                     IO.hFlush IO.stdout
                                     bufMode <- IO.hGetBuffering IO.stdin
                                     IO.hSetBuffering IO.stdin IO.NoBuffering
                                     input <- getUntil empty
                                     IO.hSetBuffering IO.stdin bufMode
                                     return $ input >$> reverse)
   where
      getUntil acc = do c <- getChar
                        if c == abortChar then return Nothing
                        else if c == '\n' then return $ Just acc
                        else                   getUntil (cons c acc)

-- Askers
-------------------------------------------------------------------------------

-- |An error message indicating that a value wasn't able to be parsed.
type TypeErrorMsg = Text
-- |An error message indicating that a value failied a predicate.
type PredicateErrorMsg = Text
-- |A prompt.
type PromptMsg = Text

-- |A predicate which a value has to fulfil.
type Predicate m a = a -> m Bool

-- |A parser which either returns a parsed value or an error message.
type Parser a = Text -> Either Text a

-- |The description of an \'ask for user input\'-action.
--  The type parameters are the used monad (typically 'IO' or 'ExceptT'),
--  the type of the read value and the type of the error that is thrown
--  in case of failures.
--
--  The components are a prompt, a parser, and a predicate that
--  the parsed value must fulfil. The predicate, being monadic, can
--  perform arbitrarily complex tests, such as checking whether a given
--  date is in the future, whether an item is in a database, whether
--  a file with a given name exists, etc.
data Asker m a = Asker{ -- |The prompt to be displayed to the user.
                        askerPrompt::Text,
                        -- |The parser for the input value.
                        askerParser::Parser a,
                        -- |The predicate which the input, once read,
                        --  must fulfill. The Left side is an error message.
                        askerPredicate::a -> m (Either Text ())}


-- |Represents a failure during the running of an asking function.
--  Either the input was incorrect in some way, or the process was aborted
--  by the user.
data AskFailure = -- |The input wasn't able to be parsed.
                  TypeFailure TypeErrorMsg
                  -- |The parsed value failed a predicate.
                  | PredicateFailure PredicateErrorMsg
                  -- |An incorrect number of parameters was passed.
                  | ParamFailure Text
                  -- |No action was appropriate for the given input.
                  | NothingFoundFailure
                  -- |The input was aborted by the user.
                  | AbortFailure
   deriving (Typeable, Eq)

instance Exception AskFailure

instance Show AskFailure where
   show (ParamFailure t) = T.unpack t
   show NothingFoundFailure = "No appropriate action found!"
   show (PredicateFailure t) = T.unpack t
   show (TypeFailure t) = T.unpack t
   show AbortFailure = "Input aborted."

-- |A verbatim Text whose Read instance simply returns the read
--  string, as-is.
--  This is useful for askers which ask for strings without quotes.
newtype Verbatim = Verbatim{fromVerbatim::Text}

-- |Read-instance for 'Verbatim'. Wraps the given value into quotes and
--  reads it a a 'Text'.
instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ T.pack s,"")]

-- |Creates a general 'Asker' with a custom parsing function and a predicate
--  that the parsed value has to pass. If either the parsing or the predicate
--  fail, one of the given error messages is displayed.
askerP :: (Monad m, Functor m)
       => PromptMsg
       -> PredicateErrorMsg
       -> Parser a
       -> Predicate m a
       -> Asker m a
askerP pr errP parse pred = Asker pr parse check
   where
      check = pred >=$> (\case True  -> Right ()
                               False -> Left errP)

-- |Creates an 'Asker' which only cares about the type of the input.
typeAskerP :: (Monad m, Functor m)
            => PromptMsg
            -> Parser a
            -> Asker m a
typeAskerP pr parse = askerP pr undefined parse (const $ return True)

-- |An asker which asks for an optional value. If only whitespace
--  is entered (according to 'Data.Char.isSpace'), it returns 'Nothing'
--  without further parsing or checking; otherwise, it behaves identically
--  to 'asker'.
maybeAskerP :: (Monad m, Functor m)
           => PromptMsg
           -> PredicateErrorMsg
           -> Parser a
           -> Predicate m a
           -> Asker m (Maybe a)
maybeAskerP pr errP parse pred = Asker pr parse' check
   where
      parse' t = if T.all isSpace t then Right Nothing
                                    else right Just $ parse t

      check Nothing = return $ Right ()
      check (Just t) = pred t >$> (\case True  -> Right ()
                                         False -> Left errP)

-- Parsers based on Read
-------------------------------------------------------------------------------

-- |A parser based on 'Text.Read.readMaybe'. This suffices for the parsing of
--  most data types.
readParser :: Read a
           => TypeErrorMsg
           -> Text
           -> (Either Text a)
readParser errT = maybe (Left errT) Right . readMaybe . T.unpack

-- |Creates a general 'Asker' with 'Text.Read.readMaybe' as its parser.
--  Using 'Data.Read.readMaybe' is perfectly fine for most values, but it has
--  two drawbacks:
--
--  1. The user input is unpacked into a String and then parsed. This can
--     incur a performance hit for large inputs.
--  2. A Read-instance must be available for the expected type.
asker :: (Monad m, Functor m, Read a)
      => PromptMsg
      -> TypeErrorMsg
      -> PredicateErrorMsg
      -> Predicate m a
      -> Asker m a
asker pr errT errP pred = askerP pr errP (readParser errT) pred

-- |Creates an 'Asker' based on Read which just cares about the type of the input.
typeAsker :: (Monad m, Functor m, Read a)
          => PromptMsg
          -> TypeErrorMsg
          -> Asker m a
typeAsker p errT = asker p errT undefined (const $ return True)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'. The input
--  thus only has to pass a predicate, not any parsing.
predAsker :: (Monad m, Functor m)
          => PromptMsg
          -> Text -- ^Predicate error message.
          -> (Text -> m Bool) -- ^The predicate.
          -> Asker m Verbatim
predAsker p errP f = asker p (error "Type error in predAsker. This is a bug.")
                           errP (f . fromVerbatim)

-- |An asker based on Read which asks for an optional value.
maybeAsker :: (Monad m, Functor m, Read a)
           => PromptMsg
           -> TypeErrorMsg
           -> PredicateErrorMsg
           -> Predicate m a
           -> Asker m (Maybe a)
maybeAsker pr errT errP pred = maybeAskerP pr errP (readParser errT) pred

-- Running askers
--------------------------------------------------------------------------------

-- |Executes an Asker. If the process fails, an exception is thrown
--  The canonical instance of @MonadError SomeException@ is the 'ExceptT' monad.
ask :: (MonadIO m, MonadError SomeException m, Functor m)
    => Asker m a
    -> Maybe Text
    -> m a
ask a v = askEither a v >>= either (throwError . SomeException) return

-- |See 'ask'. Always reads the input from stdin.
--
-- @
-- ask' a = ask a Nothing
-- @
ask' :: (MonadIO m, MonadError SomeException m, Functor m)
     => Asker m a
     -> m a
ask' a = ask a Nothing

-- |Executes an 'Asker'. If the Text argument is Nothing, the user is asked
--  to enter a line on stdin. If it is @Just x@, @x@ is taken to be input.
askEither :: (MonadIO m, Functor m)
          => Asker m a
          -> Maybe Text
          -> m (Either AskFailure a)
askEither a = maybe getInput check
   where
      getInput = promptAbort '\ESC' (askerPrompt a)
                 >>= maybe (return $ Left AbortFailure) check

      check inp = case askerParser a inp of
         Left err -> return $ Left $ TypeFailure err
         Right t -> askerPredicate a t
                    >>= return . (Left . PredicateFailure ||| Right . const t)

-- |Repeatedly executes an ask action until the user enters a valid value.
--  Error messages are printed each time.
untilValid :: (MonadIO m, MonadError SomeException m, Functor m, Read a)
           => m a
           -> m a
untilValid m = m `catchError` (\l -> liftIO (putStrLn (show l)) >> untilValid m)
