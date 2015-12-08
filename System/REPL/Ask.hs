{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |Asking the user for input on the console.
--
--  The main type is 'Asker', which takes care of parsing
--  and verifying user input.
module System.REPL.Ask (
   -- *Types
   PromptMsg,
   TypeError,
   PredicateError,
   Predicate,
   Predicate',
   Parser,
   Asker(..),
   Asker',
   -- ** Exceptions
   AskFailure(..),
   GenericTypeFailure(..),
   GenericPredicateFailure(..),
   genericTypeFailure,
   genericPredicateFailure,
   -- * Creating askers
   -- |These are all just convenience functions.
   --  You can also create 'Asker's directly via the constructor.
   --
   --  For errors, you can supply a custom exception or use 'GenericTypeFailure', 'GenericPredicateFailure'.
   typeAskerP,
   maybeAskerP,
   -- **Creating askers via 'Read'
   -- |These askers use 'Text.Read.readMaybe' as their parser.
   --
   --  It is possible to ask for Strings, but then quotes will be required
   --  around them (per their Read-instance). To get the user's
   --  input as-is, use the 'Verbatim' type or 'predAsker'.
   Verbatim(..),
   readParser,
   asker,
   lineAsker,
   typeAsker,
   predAsker,
   maybeAsker,
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
   -- *Creating predicates
   boolPredicate,
   -- *Example askers
   -- |A few askers for convenience.
   PathRootDoesNotExist(..),
   PathIsNotWritable(..),
   filepathAsker,
   writablefilepathAsker,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, reverse)

import Control.Arrow (right, (|||))
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (isSpace)
import Data.Functor.Monadic
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified System.IO.Error as ERR
import System.REPL.Prompt
import System.REPL.Types
import Text.Read (readMaybe)

-- Askers
-------------------------------------------------------------------------------

-- |Creates an 'Asker' which only cares about the type of the input.
typeAskerP :: Applicative m
           => PromptMsg
           -> Parser a
           -> Asker' m a
typeAskerP pr parse = Asker pr parse (pure . Right)

-- |An asker which asks for an optional value. If only whitespace
--  is entered (according to 'Data.Char.isSpace'), it returns 'Nothing'
--  without further parsing or checking; otherwise, it behaves identically
--  to 'asker'.
maybeAskerP :: Applicative m
            => PromptMsg
            -> Parser a
            -> Predicate m a b
            -> Asker m (Maybe a) (Maybe b)
maybeAskerP pr parse pred = Asker pr parse' check
   where
      parse' t = if T.all isSpace t then Right Nothing
                                    else right Just $ parse t

      check Nothing = pure $ Right Nothing
      check (Just t) = pred t >$> (\case Right t -> Right (Just t)
                                         Left err -> Left err)

-- Parsers based on Read
-------------------------------------------------------------------------------

-- |A parser based on 'Text.Read.readMaybe'. This suffices for the parsing of
--  most data types.
readParser :: Read a
           => (T.Text -> TypeError)
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
--  @Asker m Verbatim@ or use 'predAsker'/'lineAsker'.
asker :: (Functor m, Read a)
      => PromptMsg
      -> (T.Text -> TypeError)
      -> Predicate' m a
      -> Asker' m a
asker pr errT pred = Asker pr (readParser errT) pred

-- |Creates an 'Asker' based on Read which just cares about the type of the input.
typeAsker :: (Applicative m, Read a)
          => PromptMsg
          -> (T.Text -> TypeError)
          -> Asker' m a
typeAsker p errT = asker p errT (pure . Right)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'.
--  Quotes around the input are not required.
--  The input thus only has to pass a predicate, not any parsing.
predAsker :: (Functor m)
          => PromptMsg
          -> Predicate m T.Text b
          -> Asker m T.Text b
predAsker pr f = Asker pr Right f

-- |A wrapper aroung 'getLine'. Prints no prompt and returns the user input as-is.
lineAsker :: Applicative m
          => Asker' m T.Text
lineAsker = predAsker "" (pure . Right)

-- |An asker based on Read which asks for an optional value.
maybeAsker :: (Applicative m, Read a)
           => PromptMsg
           -> (T.Text -> TypeError)
           -> Predicate' m a
           -> Asker' m (Maybe a)
maybeAsker pr errT pred = maybeAskerP pr (readParser errT) pred

-- Running askers
--------------------------------------------------------------------------------

-- |Executes an Asker. An 'AskFailure' is thrown if the inpout can't be
--  parsing into a value of the correct type or if the input fails the 'Asker''s
--  predicate.
ask :: (MonadIO m, MonadCatch m)
    => Asker m a b
    -> Maybe T.Text
    -> m b
ask a v = askEither a v >>= either throwM return

-- |See 'ask'. Always reads the input from stdin.
--
-- @
-- ask' a = ask a Nothing
-- @
ask' :: (MonadIO m, MonadCatch m)
     => Asker m a b
     -> m b
ask' a = ask a Nothing

-- |Executes an 'Asker'. If the Text argument is Nothing, the user is asked
--  to enter a line on stdin. If it is @Just x@, @x@ is taken to be input.
askEither :: (MonadIO m, MonadCatch m)
          => Asker m a b
          -> Maybe T.Text
          -> m (Either AskFailure b)
askEither a = maybe getInput check
   where
      getInput = (promptAbort '\ESC' (askerPrompt a) >>= check)
                 `catch` (return . Left)

      check inp = case askerParser a inp of
         Left err -> return $ Left $ TypeFailure err
         Right t -> askerPredicate a t
                    >>= return . (Left . PredicateFailure ||| Right)

-- |Repeatedly executes an ask action until the user enters a valid value.
--  Error messages are printed each time.
untilValid :: forall m a.(MonadIO m, MonadCatch m, Read a)
           => m a
           -> m a
untilValid m = m `catch` handler
   where
      handler :: AskFailure -> m a
      handler l = liftIO (putStrLn $ show l) >> untilValid m

-- Creating predicates
-------------------------------------------------------------------------------

-- |Creates a predicate from a boolean function and an error message.
boolPredicate :: Functor m
              => (a -> m Bool)
              -> (a -> PredicateError)
              -> Predicate m a a
boolPredicate f errP t = (\case {True -> Right t; False -> Left (errP t)}) <$> f t

-- Example askers
-------------------------------------------------------------------------------

-- |Asks the user for a file or a directory.
-- 
--  Parsing checks for basic validity via 'System.FilePath.isValid'. Invalid paths are rejected.
--
--  After that, the asker determines whether the target exists and what type
--  it has. You can run a predicate on that information.
filepathAsker :: MonadIO m
              => PromptMsg
              -> (FilePath -> TypeError)
              -> Predicate m (PathExistenceType, FilePath) b
              -> Asker m FilePath b
filepathAsker pr errT pred = Asker pr parse pred'
   where
      parse = (\fp -> if FP.isValid fp then Right fp else Left $ errT fp) . T.unpack

      pred' fp = do
         exType <- liftIO $ getExistenceType fp
         pred (exType, fp)
         --return $ if ok then Right (exType, fp)
         --         else Left $ errP (exType, fp)

      getExistenceType :: FilePath -> IO PathExistenceType
      getExistenceType fp = do
         isDir <- D.doesDirectoryExist fp
         if isDir then return IsDirectory
         else do isFile <- D.doesFileExist fp
                 return $ if isFile then IsFile
                                    else DoesNotExist

-- |See 'filepathAsker'. This 'Asker' also ensures that the given path
--  is writeable in the following sense:
--
--  * at least some initial part of the path exists and
--  * the last existing part of the path is writeable.
--
--  'PathRootDoesNotExist' and 'PathIsNotWritable' exceptions are thrown if the
--  first or second of these conditions is violated.
--
--  For relative paths, we only check that the current directory is writable.
--
--  Handled exceptions:
--
--  * 'System.IO.Error.isPermissionError'
--  * 'System.IO.Error.isDoesNotExistError'
writablefilepathAsker
   :: MonadIO m
   => PromptMsg
   -> (FilePath -> TypeError)
   -> Predicate m (PathExistenceType, FilePath) b
   -> Asker m FilePath b
writablefilepathAsker pr errT pred = filepathAsker pr errT pred'
   where
      permError e = if ERR.isPermissionErrorType (ERR.ioeGetErrorType e) ||
                       ERR.isDoesNotExistErrorType (ERR.ioeGetErrorType e)
                    then Just () else Nothing
      conc :: [FilePath] -> FilePath
      conc = L.foldl' (FP.</>) ""
      doesExist fp = (||) <$> D.doesDirectoryExist (conc fp) <*> D.doesFileExist (conc fp)

      isWritable fp = catchJust permError (fp >>= D.getPermissions >$> D.writable) (const $ return False)

      -- A utility function which gets a bool and returns the second argument if its value is false,
      -- and the third if its true.
      boolEither :: (Monad m, Exception a) => (m Bool) -> a -> m (Either SomeException b) -> m (Either SomeException b)
      boolEither x falseCase trueCase = x >>= (\case{True -> trueCase; False -> return $ Left $ SomeException falseCase})

      pred' args@(_, fp) = 
         if FP.isRelative fp then boolEither (liftIO $ isWritable D.getCurrentDirectory) (PathIsNotWritable fp) (pred args)
         else do
            existingRoot <- liftIO $ takeWhile snd <$> mapM (\x -> (x,) <$> doesExist x) (L.inits $ FP.splitDirectories fp)
            if null existingRoot then return (Left $ SomeException $ PathRootDoesNotExist fp)
            else boolEither (liftIO $ isWritable (return . conc . fst . last $ existingRoot)) (PathIsNotWritable fp) (pred args)
