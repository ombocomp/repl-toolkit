{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Provides Commands for REPLs. Commands take care of input
--  and parameter-handling, and allow parameters to be supplied
--  in the same line as the command's name (e.g. ":cmd param1 param2" on stdin).
--  Provided parameters can be parsed and checked (say, against databases)
--  before they are passed to the actual command function.
--  They are relatively large units of abstraction, but they allow the easy
--  creation of relatively sophisticated command loops, and have the advantage
--  that one doesn't need to fiddle around with input handling in the middle
--  of the actual command code.
module System.REPL.Command (
   -- *Command dispatch
   -- |Using the 'Command' class is not necessary, but it makes dealing with
   --  user input considerably easier. When a command is run with a line of
   --  input, it automatically segments it by whitespace, tries to interpret
   --  each part as one of its arguments and passes them to the actual command
   --  function. If any arguments haven't been supplies, it asks for them on
   --  stdin. If too many arguments have been supplied, or if any argument'
   --  parsing returns an error, the command is aborted.
   --
   --  Example:
   --
   --  > cd = makeCommand1 ...
   --
   --  >>> :cd ../
   --  Directory changed!
   --  >>> :cd
   --  Enter new directory:
   --  >>> ../
   --  Directory changed!
   Command(..),
   commandInfo,
   runOnce,
   commandDispatch,
   summarizeCommands,
   readArgs,
   quoteArg,
   -- ** Making commands.
   makeCommand,
   makeCommand1,
   makeCommand2,
   makeCommand3,
   makeCommand4,
   makeCommand5,
   makeCommand6,
   makeCommandN,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++),
                       length, replicate)
import qualified Prelude as P

import Control.Arrow (left)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops (unfoldrM)
import Data.Char (isSpace)
import Data.Functor.Monadic
import qualified Data.List as LU
import qualified Data.List.Safe as L
import Data.ListLike(ListLike(..))
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Ord
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Typeable
import Numeric.Peano
import System.REPL
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

-- alias for Data.ListLike.append
(++) :: (ListLike full item) => full -> full -> full
(++) = append

-- |A REPL command, possibly with parameters.
data Command m a = Command{
                  -- |The short name of the command. Purely informative.
                  commandName :: Text,
                  -- |Returns whether a string matches
                  --  a command name. The simplest form is
                  --  @s==@ for some string s, but more liberal
                  --  matchings are possible.
                  commandTest :: Text -> Bool,
                  -- |A description of the command.
                  commandDesc :: Text,
                  -- |The number of parameters, if fixed.
                  numParameters :: Maybe Int,
                  -- |Runs the command with the input text as parameter.
                  runCommand :: Text -> m a}

instance Functor m => Functor (Command m) where
   fmap f c@Command{runCommand=run} = c{runCommand=(fmap f . run)}

data ParamNumError = NoParams | ExactParams | TooManyParams
   deriving (Enum, Show, Eq, Read, Typeable, Ord)

instance Exception ParamNumError

-- |Prints information (the command name, description and, if given,
--  the number of parameters) about a command to the console.
commandInfo :: MonadIO m => Command m a -> m ()
commandInfo c = liftIO $ do
   putStr $ commandName c
   putStrLn $ maybe "" ((" Parameters: " P.++) . show) (numParameters c)
   putStrLn $ commandDesc c

-- |Splits and trims the input of a command.
--  Any non-whitespace sequence of characters is interpreted as
--  one argument, unless double quotes (") are used, in which case
--  they demarcate an argument. Each argument is parsed as a haskell
--  string literal (quote-less arguments have quotes inserted around them).
--  If the number of quotes in the input is not even, the operating will fail.
--
--  Arguments are parsed using parsec's @stringLiteral@ (haskell-style),
--  meaning that escape sequences and unicode characters are handled automatically.
readArgs :: Text -> Either Text [Text]
readArgs = (left $ T.pack . show) . P.parse parser "" . T.unpack
   where
      -- Main parser.
      parser = P.many (stringLiteral P.<|> unquotedLiteral)

      stringLiteral = P.stringLiteral P.haskell >$> T.pack

      -- The parser for string literals without quotes around them.
      --
      -- First we read a bunch of characters and then we pass the result,
      -- wrapped in quotes, to the stringLiteral parser AGAIN.
      -- This might seem strange, but this way, escape sequences are correctly
      -- handled. The alternative would have been to copy the (private) logic
      -- found in Text.Parsec.Token's source.
      unquotedLiteral =
         do raw <- P.many1 $ P.satisfy $ not . isSpace
            P.eof P.<|> (P.many1 P.space >> return ())
            let lit = stringLiteral
                res = P.parse lit "" ("\"" ++ raw ++ "\"")
            case res of (Right r) -> return r
                        (Left l) -> fail (show l)

-- |Takes a line of text and a command.
--  If the text matches the given command's 'commandTest',
--  the command is run with it. If not, 'Nothing' is returned.
runOnce :: MonadIO m => Text -> Command m a -> m (Maybe a)
runOnce l c = if commandTest c l then liftM Just (runCommand c l)
                                 else return Nothing


-- |Returns an error message if an unexpected number of parameters have been
--  supplied.
paramErr :: Text -- ^The command name.
         -> [Text] -- ^The given input.
         -> Int  -- ^The minimum number of parameters.
         -> Nat  -- ^The maximum number of parameters. May be infinite if there
                 --  is no upper bound.
         -> ParamNumError -- ^The kind of error that occurred.
         -> Text
paramErr c inp minNum maxNum errType =
   "The following " ++ T.pack (show num) ++ " parameters were given to " ++ c ++ ":\n"
   ++ T.intercalate " " (maybe [] (L.map wrap) $ L.tail inp) ++ ".\n"
   ++ (numErr LU.!! fromEnum errType)
   where
      -- wraps the argument in quotation marks if it contains a space
      wrap t = if T.any isSpace t then "\"" ++ t ++ "\"" else t
      -- number of arguments (excluding the command name)
      num = L.length inp - 1
      -- error message regarding how many parameters the command takes
      numErr = [c ++ " takes no parameters.",
                c ++ " takes " ++ T.pack (show minNum) ++ " parameters.",
                c ++ " takes at most " ++ T.pack (show (fromPeano maxNum :: Integer)) ++ " parameters."]

-- |Checks the number of parameters before executing a monadic function.
--  Only AskFailures (and IOExceptions) will be thrown in this function.
checkParams :: (MonadIO m, MonadThrow m, Functor m)
            => Text -- ^The command name.
            -> Text -- ^The raw input (including the command name).
            -> Int -- ^The minimal number of parameters, excluding the command's name.
            -> Nat -- ^The maximal number of parameters, excluding the command's name.
                   --  This may be infinity if there is no upper bound.
            -> ([Text] -> m a) -- ^The command.
            -> m a -- ^Result. If too many parameters were
                   --  passed, this will be a 'ParamNumFailure'.
checkParams n inp minNum maxNum m =
   case readArgs inp of
      Left l  -> throwM (ParamFailure l)
      Right r ->
         if natLength r > maxNum + 1 then
            throwM $ ParamFailure
                   $ paramErr n r minNum maxNum (errKind $ natLength r)
         else m r
   where
      errKind len = if minNum == 0 && 0 == maxNum then NoParams
                    else if maxNum < len then TooManyParams
                    else ExactParams

-- |Surrounds an argument in quote marks, if necessary.
--  This is useful when arguments were extracted via 'readArgs', which deletes
--  quote marks. Quotes are placed around the input iff it doesn't begin with
--  a quote mark (\").
--  'readArgs' and 'quoteArg' are inverse up to suitable isomorphism, i.e.
--  if 'readArgs orig = (Right res)', then it holds that
--  @readArgs orig = readArgs $ intercalate " " $ map quoteArg res@
quoteArg :: Text -> Text
quoteArg x = if T.null x || T.head x /= '\"'
                then '\"' `T.cons` x `T.snoc` '\"'
                else x

-- |Creates a command without parameters.
makeCommand :: (MonadIO m, MonadCatch m,
                Functor m)
            => Text -- ^Command name.
            -> (Text -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> (Text -> m a) -- ^The actual command.
            -> Command m a
makeCommand n t d f =
   Command n t d (Just 0) (\inp -> checkParams n inp 0 0 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 f li

-- |Creates a command with one parameter.
makeCommand1 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> (Text -> a -> m z)
             -> Command m z
makeCommand1 n t d p1 f =
   Command n t d (Just 1) (\inp -> checkParams n inp 1 1 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 f li x1

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> (Text -> a -> b -> m z)
             -> Command m z
makeCommand2 n t d p1 p2 f =
   Command n t d (Just 2) (\inp -> checkParams n inp 2 2 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 x2 <- ask p2 (inp L.!! 2)
                 f li x1 x2

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> (Text -> a -> b -> c -> m z)
             -> Command m z
makeCommand3 n t d p1 p2 p3 f =
   Command n t d (Just 3) (\inp -> checkParams n inp 3 3 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 x2 <- ask p2 (inp L.!! 2)
                 x3 <- ask p3 (inp L.!! 3)
                 f li x1 x2 x3

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> (Text -> a -> b -> c -> d -> m z)
             -> Command m z
makeCommand4 n t d p1 p2 p3 p4 f =
   Command n t d (Just 4) (\inp -> checkParams n inp 4 4 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 x2 <- ask p2 (inp L.!! 2)
                 x3 <- ask p3 (inp L.!! 3)
                 x4 <- ask p4 (inp L.!! 4)
                 f li x1 x2 x3 x4

-- |Creates a command with five parameters.
makeCommand5 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> (Text -> a -> b -> c -> d -> e -> m z)
             -> Command m z
makeCommand5 n t d p1 p2 p3 p4 p5 f =
   Command n t d (Just 4) (\inp -> checkParams n inp 5 5 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 x2 <- ask p2 (inp L.!! 2)
                 x3 <- ask p3 (inp L.!! 3)
                 x4 <- ask p4 (inp L.!! 4)
                 x5 <- ask p5 (inp L.!! 5)
                 f li x1 x2 x3 x4 x5

-- |Creates a command with four parameters.
makeCommand6 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> Asker m f -- ^'Asker' for the sixth parameter.
             -> (Text -> a -> b -> c -> d -> e -> f -> m z)
             -> Command m z
makeCommand6 n t d p1 p2 p3 p4 p5 p6 f =
   Command n t d (Just 4) (\inp -> checkParams n inp 6 6 c)
   where
      c inp = do let li = maybe "" id (L.head inp)
                 x1 <- ask p1 (inp L.!! 1)
                 x2 <- ask p2 (inp L.!! 2)
                 x3 <- ask p3 (inp L.!! 3)
                 x4 <- ask p4 (inp L.!! 4)
                 x5 <- ask p5 (inp L.!! 5)
                 x6 <- ask p6 (inp L.!! 6)
                 f li x1 x2 x3 x4 x5 x6

-- |Creates a command with a list of parameters.
--  The first list @necc@ of 'Asker's indicates the necessary parameters;
--  the user must at least provide this many. The second list @opt@ contains
--  'Asker's for additional, optional parameters, and may be infinite.
--  If the number of passed parameters exceeds
--  @length necc + length opt@, or if any 'Asker' fails,
--  the command returns an 'AskFailure'.
makeCommandN :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> [Asker m a] -- ^'Asker's for the necessary parameters.
             -> [Asker m a] -- ^'Asker's for the optional parameters.
             -> (Text -> [a] -> m z)
             -> Command m z
makeCommandN n t d necc opt f = Command n t d Nothing (\inp -> checkParams n inp min max c)
   where
      min = P.length necc
      max = natLength necc + natLength opt

      c inp = do let li = maybe "" id (L.head inp)
                 neccParams <- unfoldrM (comb inp) (necc,1, Nothing)
                 let from = L.length neccParams + 1
                     to = Just $ L.length inp - 1

                 optParams <- unfoldrM (comb inp) (opt, from, to)
                 f li (neccParams L.++ optParams)

      -- |Goes through the list of askers until all are done or until the first
      --  AskFailure occurs. The results are of type @Either (AskFailure e) z@,
      --  the state is of type @([Asker m a e], Int)@. The second component @i@
      --  indicates that the @i@th parameter is to be read.
      comb _ ([],_,_) = return Nothing
      comb inp (x:xs, i, j) =
         if isJust j && fromJust j < i then return Nothing
         else ask x (inp L.!! i) >$> args xs >$> Just

         where args ys y = (y,(ys,i+1,j))

-- |Takes an input and tries to run it against a list of commands,
--  trying the out in sequence. The first command whose 'commandTest'
--  returns True is executed. If none of the commands match,
--  @NothingFoundFailure@ is thrown.
commandDispatch :: (MonadIO m, MonadCatch m, Functor m)
                => Text -- ^The user's input.
                -> [Command m z] -- ^The command library.
                -> m z
commandDispatch input cs =
   case readArgs input of
      Left l -> throwM (ParamFailure l)
      Right input' -> if noMatch input'
                      then throwM NothingFoundFailure
                      else do runCommand (fromJust $ first input') input
   where
      noMatch = isNothing . first
      firstArg = maybe "" id . L.head
      first r = L.head $ P.dropWhile (not . flip commandTest (firstArg r)) cs


-- |Prints out a list of command names, with their descriptions.
summarizeCommands :: MonadIO m
                  => [Command m2 z]
                  -> m ()
summarizeCommands [] = return ()
summarizeCommands xs = liftIO $ mapM_ (\c -> prName c >> prDesc c) xs
   where
      maxLen :: Int
      maxLen = fromIntegral
               $ T.length
               $ commandName
               $ fromJust
               $ L.minimumBy (comparing $ (* (-1)) . T.length . commandName) xs
      prName = putStr . padRight ' ' maxLen . commandName
      prDesc = putStrLn . (" - " ++) . commandDesc

      padRight c i cs = cs ++ replicate (i - length cs) c
