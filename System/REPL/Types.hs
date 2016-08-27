-- |Types used by other modules in the package.
--
--  The module contains the following exception hierarchy:
--
--  * 'SomeREPLError'
--
--      * 'SomeAskerError'
--
--          * 'AskerTypeError'
--          * 'AskerPredicateError'
--          * 'AskerInputAbortedError'
--
--      * 'SomeCommandError'
--
--          * 'MalformedParamsError'
--          * 'TooFewParamsError'
--          * 'TooManyParamsError'
--
--  * 'NoConfigFileParseError'
--
module System.REPL.Types where

import Control.Exception (SomeException(..), Exception(..))
import qualified Data.Functor.Apply as Ap
import qualified Data.Functor.Bind as Bi
import qualified Data.Text as T
import Data.Typeable
import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJClass as PPH

-- Asker types
-------------------------------------------------------------------------------

-- |An error message indicating that a value wasn't able to be parsed.
type TypeError = SomeException
-- |An error message indicating that a value failed a predicate.
type PredicateError = SomeException
-- |A prompt.
type PromptMsg = T.Text

-- |A predicate which a value has to fulfil.
type Predicate m a b = a -> m (Either PredicateError b)

-- |A predicate which does not change the type of its input.
type Predicate' m a = Predicate m a a

-- |A parser which either returns a parsed value or an error message.
type Parser a = T.Text -> Either TypeError a

-- |The description of an \'ask for user input\'-action.
--  The type parameters are the used monad (typically 'IO' or 'ExceptT'),
--  the type of the read value and the type of the error that is thrown
--  in case of failures.
--
--  The components are a prompt, a parser, and a predicate that
--  the parsed value must fulfil. The predicate
--
--  * is monadic and
--  * can change the returned type (useful for adjoining additional information)
data Asker m a b = Asker{ -- |The prompt to be displayed to the user.
                          askerPrompt::T.Text,
                          -- |The parser for the input value.
                          askerParser::Parser a,
                          -- |The predicate which the input, once read,
                          --  must fulfill. The Left side is an error message.
                          askerPredicate::Predicate m a b}

-- |An Asker which does not convert its argument into a different type after parsing.
type Asker' m a = Asker m a a

-- |Root of the exception hierarchy.
data SomeREPLError = forall e.Exception e => SomeREPLError e deriving (Typeable)
instance Show SomeREPLError where show (SomeREPLError e) = show e
instance Exception SomeREPLError

replErrorUpcast :: (Exception a) => a -> SomeException
replErrorUpcast = toException . SomeREPLError
replErrorDowncast :: (Exception a) => SomeException -> Maybe a
replErrorDowncast x = do {SomeREPLError y <- fromException x; cast y}

-- |Generic error related to 'Asker's. Either the input was incorrect
--  in some way, or the process was aborted by the user.
data SomeAskerError = forall e.Exception e => SomeAskerError e deriving (Typeable)
instance Show SomeAskerError where show (SomeAskerError e) = show e
instance Exception SomeAskerError where
   toException = replErrorUpcast
   fromException = replErrorDowncast

askerErrorUpcast :: (Exception a) => a -> SomeException
askerErrorUpcast = toException . SomeAskerError
askerErrorDowncast :: (Exception a) => SomeException -> Maybe a
askerErrorDowncast x = do {SomeAskerError y <- fromException x; cast y}

-- |The input could not be parsed.
data AskerTypeError = AskerTypeError SomeException deriving (Show, Typeable)
instance Exception AskerTypeError where
   toException = askerErrorUpcast
   fromException = askerErrorDowncast

-- |The parsed value failed a predicate.
data AskerPredicateError = AskerPredicateError SomeException deriving (Show, Typeable)
instance Exception AskerPredicateError where
   toException = askerErrorUpcast
   fromException = askerErrorDowncast

-- |The input for an Asker was aborted by the user.
data AskerInputAbortedError = AskerInputAbortedError deriving (Show, Typeable)
instance Exception AskerInputAbortedError where
   toException = askerErrorUpcast
   fromException = askerErrorDowncast
-- |Prints "Input aborted."
instance PPH.Pretty AskerInputAbortedError where
   pPrint AskerInputAbortedError = PP.text "Input aborted."

-- |A generic type failure for use with Askers.
data GenericTypeError = GenericTypeError T.Text deriving (Show, Typeable, Eq)
instance Exception GenericTypeError
instance PPH.Pretty GenericTypeError where
   pPrint (GenericTypeError t) = PP.text $ T.unpack t

-- |Constructor for 'GenericTypeError' which wraps the value into a 'SomeException'.
genericTypeError :: T.Text -> SomeException
genericTypeError = SomeException . GenericTypeError

-- |A generic predicate failure for use with Askers.
data GenericPredicateError = GenericPredicateError T.Text deriving (Show, Typeable, Eq)
instance Exception GenericPredicateError
instance PPH.Pretty GenericPredicateError where
   pPrint (GenericPredicateError t) = PP.text $ T.unpack t

-- |Constructor for 'GenericTypeError' which wraps the value into a 'SomeException'.
genericPredicateError :: T.Text -> SomeException
genericPredicateError = SomeException . GenericPredicateError

-- |A verbatim Text whose Read instance simply returns the read
--  string, as-is.
--  This is useful for askers which ask for strings without quotes.
newtype Verbatim = Verbatim{fromVerbatim::T.Text}

-- |Read-instance for 'Verbatim'. Wraps the given value into quotes and
--  reads it a a 'T.Text'.
instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ T.pack s,"")]

-- Types for example askers
-------------------------------------------------------------------------------

-- |Indicates whether the target of a path exists and what form it has.
data PathExistenceType = IsDirectory | IsFile | DoesNotExist deriving (Eq, Show, Ord, Read, Enum, Bounded)

-- |Indicates that no part of a path exists.
data PathRootDoesNotExist = PathRootDoesNotExist FilePath deriving (Typeable, Eq, Show)
instance Exception PathRootDoesNotExist

-- |Indicates that the last existing portion of a path is not writable.
data PathIsNotWritable = PathIsNotWritable FilePath deriving (Typeable, Eq, Show)
instance Exception PathIsNotWritable

-- Command types
-------------------------------------------------------------------------------

-- Exceptions
-------------------------------------------------------------------------------

-- |Generic error related to command execution.
data SomeCommandError = forall e.Exception e => SomeCommandError e deriving (Typeable)
instance Show SomeCommandError where show (SomeCommandError e) = show e
instance Exception SomeCommandError where
   toException = replErrorUpcast
   fromException = replErrorDowncast

commandErrorUpcast :: (Exception a) => a -> SomeException
commandErrorUpcast = toException . SomeCommandError
commandErrorDowncast :: (Exception a) => SomeException -> Maybe a
commandErrorDowncast x = do {SomeCommandError y <- fromException x; cast y}

-- |The input of a command was malformed and could not be interpreted. I.e.
--  the input contained inadmissible characters, or quotes were mismatched.
--  The 'Text' argument contains the parser error.
data MalformedParamsError = MalformedParamsError T.Text deriving (Show, Eq, Typeable, Ord)
instance Exception MalformedParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast
-- |Prints the contained text, preceded by "Malformed parameters: "
instance PPH.Pretty MalformedParamsError where
   pPrint (MalformedParamsError t) = PP.text $ "Malformed parameters:" ++ T.unpack t

-- |Too many parameters were given to a command. The first value is the maximum,
--  the second the actual number.
data TooManyParamsError = TooManyParamsError Int Int deriving (Show, Eq, Typeable, Ord)
instance Exception TooManyParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast
-- |Prints "Too many parameters! Got <# of params>, expected at most <# of max. params>."
instance PPH.Pretty TooManyParamsError where
   pPrint (TooManyParamsError e g) =
      PP.text $ mconcat ["Too many parameters! Got ", show g,
                         ", expected at most", show e, "."]

-- |Too few parameters were given to a command. The first value is the minium,
--  the second the actual number.
data TooFewParamsError = TooFewParamsError Int Int deriving (Show, Eq, Typeable, Ord)
instance Exception TooFewParamsError where
   toException = commandErrorUpcast
   fromException = commandErrorDowncast
-- |Prints "Too few parameters! Got <# of params>, expected at least <# of max. params>."
instance PPH.Pretty TooFewParamsError where
   pPrint (TooFewParamsError e g) =
      PP.text $ mconcat ["Too many parameters! Got ", show g,
                         ", expected at least", show e, "."]

-- Command type
-------------------------------------------------------------------------------

-- |A REPL command, possibly with parameters.
data Command m i a = Command{
                     -- |The short name of the command. Purely informative.
                     commandName :: T.Text,
                     -- |Returns whether the first part of an input
                     --  (the command name) matches
                     --  a the command. 'defCommandTest' is appropriate for most cases.
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
