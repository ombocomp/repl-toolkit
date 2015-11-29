{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- |Asking the user for input on the console.
--
--  The main type is 'Asker', which takes care of parsing
--  and verifying user input.
module System.REPL.Ask (
   -- *Types
   PromptMsg,
   TypeErrorMsg,
   PredicateErrorMsg,
   Predicate,
   Parser,
   Asker(..),
   AskFailure(..),
   -- * Creating askers
   askerP,
   typeAskerP,
   maybeAskerP,
   -- **Creating askers via 'Read'
   -- |These askers use 'Text.Read.readMaybe' as their parser.
   --
   --  It is possible to ask for Strings, but then quotes will be required
   --  around them (per their Read-instance). To get the user's
   --  input as-is, use the 'Verbatim' type or 'predAsker'.
   readParser,
   asker,
   typeAsker,
   predAsker,
   maybeAsker,
   Verbatim(..),
   -- *Running askers
   -- |Created askers can be run via these functions.
   --  Since the parsing depends on the Read-instance, the expected result type
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
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (isSpace)
import Data.Functor.Monadic
import qualified Data.Text as T
import System.REPL.Prompt
import System.REPL.Types
import Text.Read (readMaybe)

-- Askers
-------------------------------------------------------------------------------

-- |Creates a general 'Asker' with a custom parsing function and a predicate
--  that the parsed value has to pass. If either the parsing or the predicate
--  fail, one of the given error messages is displayed.
askerP :: (Monad m)
       => PromptMsg
       -> (a -> PredicateErrorMsg)
       -> Parser a
       -> Predicate m a
       -> Asker m a
askerP pr errP parse pred = Asker pr parse check
   where
      check x = pred x >$> (\case True  -> Right ()
                                  False -> Left $ errP x)

-- |Creates an 'Asker' which only cares about the type of the input.
typeAskerP :: (Monad m)
            => PromptMsg
            -> Parser a
            -> Asker m a
typeAskerP pr parse = askerP pr (error "LIBRARY BUG: undefined in System.REPL.typeAskerP") parse (const $ return True)

-- |An asker which asks for an optional value. If only whitespace
--  is entered (according to 'Data.Char.isSpace'), it returns 'Nothing'
--  without further parsing or checking; otherwise, it behaves identically
--  to 'asker'.
maybeAskerP :: (Monad m)
           => PromptMsg
           -> (a -> PredicateErrorMsg)
           -> Parser a
           -> Predicate m a
           -> Asker m (Maybe a)
maybeAskerP pr errP parse pred = Asker pr parse' check
   where
      parse' t = if T.all isSpace t then Right Nothing
                                    else right Just $ parse t

      check Nothing = return $ Right ()
      check (Just t) = pred t >$> (\case True  -> Right ()
                                         False -> Left $ errP t)

-- Parsers based on Read
-------------------------------------------------------------------------------

-- |A parser based on 'Text.Read.readMaybe'. This suffices for the parsing of
--  most data types.
readParser :: Read a
           => (T.Text -> TypeErrorMsg)
           -> Parser a
readParser errT t = maybe (Left $ errT t) Right . readMaybe . T.unpack $ t

-- |Creates a general 'Asker' with 'Text.Read.readMaybe' as its parser.
--  Using 'Data.Read.readMaybe' is perfectly fine for most values, keep in mind
--  that the input Text has to be unpacked into a string. This can be costly
--  on very large inputs.
--
--  __NOTE:__ Instances of String/Text have to be surrounded with quotes (\").
--  You practically never want this when asking for input.
--  If you want to get the user input as-is, restrict the return type to
--  @Asker m Verbatim@ or use 'predAsker'.
asker :: (Monad m, Read a)
      => PromptMsg
      -> (T.Text -> TypeErrorMsg)
      -> (a -> PredicateErrorMsg)
      -> Predicate m a
      -> Asker m a
asker pr errT errP pred = askerP pr errP (readParser errT) pred

-- |Creates an 'Asker' based on Read which just cares about the type of the input.
typeAsker :: (Monad m, Read a)
          => PromptMsg
          -> (T.Text -> TypeErrorMsg)
          -> Asker m a
typeAsker p errT = asker p errT (error "LIBRARY BUG: undefined in System.REPL.typeAsker") (const $ return True)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'.
--  Quotes around the input are not required.
--  The input thus only has to pass a predicate, not any parsing.
predAsker :: (Monad m)
          => PromptMsg
          -> (T.Text -> PredicateErrorMsg)
          -> Predicate m T.Text
          -> Asker m T.Text
predAsker pr errP f = askerP pr errP Right f

-- |An asker based on Read which asks for an optional value.
maybeAsker :: (Monad m, Read a)
           => PromptMsg
           -> (T.Text -> TypeErrorMsg)
           -> (a -> PredicateErrorMsg)
           -> Predicate m a
           -> Asker m (Maybe a)
maybeAsker pr errT errP pred = maybeAskerP pr errP (readParser errT) pred

-- Running askers
--------------------------------------------------------------------------------

-- |Executes an Asker. An 'AskFailure' is thrown if the inpout can't be
--  parsing into a value of the correct type or if the input fails the 'Asker''s
--  predicate.
ask :: (MonadIO m, MonadCatch m)
    => Asker m a
    -> Maybe T.Text
    -> m a
ask a v = askEither a v >>= either throwM return

-- |See 'ask'. Always reads the input from stdin.
--
-- @
-- ask' a = ask a Nothing
-- @
ask' :: (MonadIO m, MonadCatch m)
     => Asker m a
     -> m a
ask' a = ask a Nothing

-- |Executes an 'Asker'. If the Text argument is Nothing, the user is asked
--  to enter a line on stdin. If it is @Just x@, @x@ is taken to be input.
askEither :: (MonadIO m, MonadCatch m)
          => Asker m a
          -> Maybe T.Text
          -> m (Either AskFailure a)
askEither a = maybe getInput check
   where
      getInput = (promptAbort '\ESC' (askerPrompt a) >>= check)
                 `catch` (return . Left)

      check inp = case askerParser a inp of
         Left err -> return $ Left $ TypeFailure err
         Right t -> askerPredicate a t
                    >>= return . (Left . PredicateFailure ||| Right . const t)

-- |Repeatedly executes an ask action until the user enters a valid value.
--  Error messages are printed each time.
untilValid :: forall m a.(MonadIO m, MonadCatch m, Read a)
           => m a
           -> m a
untilValid m = m `catch` handler
   where
      handler :: AskFailure -> m a
      handler l = liftIO (putStrLn $ show l) >> untilValid m
