{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Types used by other modules in the package.
module System.REPL.Types where

import Control.Exception
import qualified Data.Functor.Apply as Ap
import qualified Data.Functor.Bind as Bi
import qualified Data.Text as T
import Data.Typeable

-- Asker types
-------------------------------------------------------------------------------

-- |An error message indicating that a value wasn't able to be parsed.
type TypeErrorMsg = T.Text
-- |An error message indicating that a value failied a predicate.
type PredicateErrorMsg = T.Text
-- |A prompt.
type PromptMsg = T.Text

-- |A predicate which a value has to fulfil.
type Predicate m a = a -> m Bool

-- |A parser which either returns a parsed value or an error message.
type Parser a = T.Text -> Either T.Text a

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
                        askerPrompt::T.Text,
                        -- |The parser for the input value.
                        askerParser::Parser a,
                        -- |The predicate which the input, once read,
                        --  must fulfill. The Left side is an error message.
                        askerPredicate::a -> m (Either T.Text ())}


-- |Represents a failure during the running of an asking function.
--  Either the input was incorrect in some way, or the process was aborted
--  by the user.
data AskFailure = -- |The input wasn't able to be parsed.
                  TypeFailure TypeErrorMsg
                  -- |The parsed value failed a predicate.
                  | PredicateFailure PredicateErrorMsg
                  -- |An incorrect number of parameters was passed.
                  | ParamFailure T.Text
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
newtype Verbatim = Verbatim{fromVerbatim::T.Text}

-- |Read-instance for 'Verbatim'. Wraps the given value into quotes and
--  reads it a a 'T.Text'.
instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ T.pack s,"")]

-- Command types
-------------------------------------------------------------------------------

-- Exceptions
-------------------------------------------------------------------------------

-- |Generic error related to command execution.
data SomeCommandError = forall e.Exception e => SomeCommandError e deriving (Typeable)
instance Show SomeCommandError where show (SomeCommandError e) = show e
instance Exception SomeCommandError

commandErrorUpcast :: (Exception a) => a -> SomeException
commandErrorUpcast = toException . SomeCommandError
commandErrorDowncast :: (Exception a) => SomeException -> Maybe a
commandErrorDowncast x = do {SomeCommandError y <- fromException x; cast y}

-- |The input of a command was malformed and could not interpreted. I.e.
--  the input contained inadmissible characters, or quotes were mismatched.
--  The 'Text' argument contains the parser error.
data MalformedParamsError = MalformedParamsError T.Text deriving (Show, Eq, Typeable, Ord)
instance Exception MalformedParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast

-- |Too many parameters were given to a command. The first value is the maximum,
--  the second the actual number.
data TooManyParamsError = TooManyParamsError Int Int deriving (Show, Eq, Typeable, Ord)
instance Exception TooManyParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast

-- |Too few parameters were given to a command. The first value is the minium,
--  the second the actual number.
data TooFewParamsError = TooFewParamsError Int Int deriving (Show, Eq, Typeable, Ord)
instance Exception TooFewParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast

-- Command type
-------------------------------------------------------------------------------

-- |A REPL command, possibly with parameters.
data Command m i a = Command{
                     -- |The short name of the command. Purely informative.
                     commandName :: T.Text,
                     -- |Returns whether the first part of an input
                     --  (the command name) matches
                     --  a the command. The simplest form is
                     --  @((==) . getPart) s@ for some string s, but more liberal
                     --  matchings are possible.
                     commandTest :: i -> Bool,
                     -- |A description of the command.
                     commandDesc :: T.Text,
                     -- |Runs the command with the input text as parameter,
                     --  returning the unconsumed input.
                     runPartialCommand :: [i] -> m (a, [i])}

instance Functor m => Functor (Command m i) where
   fmap f c@Command{runPartialCommand=run} = c{runPartialCommand=(fmap (\(x,y) -> (f x, y))  . run)}

instance (Monad m) => Ap.Apply (Command m i) where
   -- |Runs the first command, then the second with the left-over input.
   --  The result of the first command is applied to that of the second.
   --
   --  All other fields (name, description,...) of the second command are
   --  ignored.
   f <.> g = f{runPartialCommand = h}
      where
         h input = do (func, output) <- runPartialCommand f input
                      (arg, output') <- runPartialCommand g output
                      return (func arg, output')


instance (Monad m) => Bi.Bind (Command m i) where
   -- |The same as 'Ap.<.>', but the second argument can read the result of the
   --  first.
   f >>- g = f{runPartialCommand = h}
      where
         h input = do (res, output)   <- runPartialCommand f input
                      (res', output') <- runPartialCommand (g res) output
                      return (res', output')


-- Config file types
-------------------------------------------------------------------------------

-- |Indicates that some string was not able to be parsed.
data NoConfigFileParseError = NoConfigFileParseError T.Text deriving (Show, Eq, Read, Typeable)

instance Exception NoConfigFileParseError
