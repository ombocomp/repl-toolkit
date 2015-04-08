{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

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
   -- *Command class
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
   runCommand,
   runSingleCommand,
   oneOf,
   subcommand,
   makeREPL,
   -- * Exceptions
   SomeCommandError(..),
   MalformedParamsError(..),
   TooFewParamsError(..),
   TooManyParamsError(..),
   -- * Dealing with arguments
   readArgs,
   getName,
   quoteArg,
   -- * Helpers
   summarizeCommands,
   -- * Making commands.
   makeCommand,
   makeCommand1,
   makeCommand2,
   makeCommand3,
   makeCommand4,
   makeCommand5,
   makeCommand6,
   makeCommand7,
   makeCommand8,
   makeCommandN,
   ) where

import Prelude hiding (putStrLn, putStr, (++), length, replicate)
import qualified Prelude as P

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops (unfoldrM, iterateUntil)
import Data.Char (isSpace)
import Data.Foldable (Foldable)
import qualified Data.Functor.Apply as Ap
import qualified Data.Functor.Bind as Bi
import Data.Functor.Monadic
import qualified Data.List as LU
import qualified Data.List.Safe as L
import Data.ListLike(ListLike(..))
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Monoid (mempty, Monoid)
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
data MalformedParamsError = MalformedParamsError Text deriving (Show, Eq, Typeable, Ord)
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
                     commandName :: Text,
                     -- |Returns whether the first part of an input
                     --  (the command name) matches
                     --  a the command. The simplest form is
                     --  @((==) . getPart) s@ for some string s, but more liberal
                     --  matchings are possible.
                     commandTest :: i -> Bool,
                     -- |A description of the command.
                     commandDesc :: Text,
                     -- |Indicates whether the command should run IO actions
                     --  to get needed input.
                     --canAsk :: Bool,
                     -- |The minium and maximum number of parameters.
                     --numParameters :: (Int, Maybe Int),
                     -- |Runs the command with the input text as parameter,
                     --  returning the unconsumed input.
                     runPartialCommand :: [i] -> m (a, [i])}

instance Functor m => Functor (Command m i) where
   fmap f c@Command{runPartialCommand=run} = c{runPartialCommand=(fmap (first f)  . run)}

instance (Functor m, Monad m) => Ap.Apply (Command m i) where
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


instance (Functor m, Monad m) => Bi.Bind (Command m i) where
   -- |The same as 'Ap.<.>', but the second argument can read the result of the
   --  first.
   f >>- g = f{runPartialCommand = h}
      where
         h input = do (res, output)   <- runPartialCommand f input
                      (res', output') <- runPartialCommand (g res) output
                      return (res', output')

-- |Runs the command with the input text as parameter, discarding any left-over
--  input.
runCommand :: (Functor m, Monad m, MonadThrow m) => Command m Text a -> Text -> m a
runCommand c = fmap fst . runPartialCommand c <=< readArgs

-- |Runs the command with the input text as parameter. If any input is left
--  unconsumed, an error is thrown.
runSingleCommand :: (MonadThrow m, Functor m) => Command m Text a -> Text -> m a
runSingleCommand c t = do
   t' <- readArgs t
   (res, output) <- runPartialCommand c t'
   let act = length t'
       mx  = act - length output
   when (not . L.null $ output) (throwM $ TooManyParamsError mx act)
   return res


-- |Takes a list @xs@ and executes the first command in a list whose
--  'commandTest' matches the input.
--
--  Note that the resultant command @c@'s' 'runPartialCommand' should only be
--  executed with an input @t@ if 'commandTest c t' == True', where @t'@ is either
--  @head (readArgs t)@ or @mempty@ if @t@ is empty.
--  Otherwise, the result is undefined.
oneOf :: Monoid i
      => Text
         -- ^Command name.
      -> Text
         -- ^Command description.
      -> [Command m i a]
      -> Command m i a
oneOf n d xs = Command n test d cmd
   where
      test t = L.any (($ t) . commandTest) xs
      -- because of @test@, the list is guaranteed to be non-empty
      cmd input = (`runPartialCommand` input)
                  . LU.head
                  . L.dropWhile (not . ($ fromMaybe mempty (L.head input)) . commandTest) $ xs

-- |Adds a list of possible subcommands after a command (that should leave
--  some input unconsumed). Ignoring all the required parameters for a moment,
--
--  > subcommand x xs = x >>- oneOf xs
subcommand :: (Functor m, Monad m, Monoid i)
           => Command m i a
              -- ^The root command.
           -> [a -> Command m i b]
              -- ^The subcommands that may follow it. This list must be finite.
           -> Command m i b
subcommand x xs = x Bi.>>- \y -> oneOf "" "" (L.map ($ y) xs)


-- |Splits and trims the input of a command. If the input cannot be parsed, a
--  'MalformedCommand' exception is thrown.
--
--  -- * Format
--
--  Any non-whitespace sequence of characters is interpreted as
--  one argument, unless double quotes (") are used, in which case
--  they demarcate an argument. Each argument is parsed as a haskell
--  string literal (quote-less arguments have quotes inserted around them).
--
--  Arguments are parsed using parsec's @stringLiteral@ (haskell-style),
--  meaning that escape sequences and unicode characters are handled automatically.
readArgs :: MonadThrow m => Text -> m [Text]
readArgs = either err return . P.parse parser "" . T.unpack
   where
      err = throwM . MalformedParamsError . T.pack . show
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

-- |Gets the first part of a command string, or the empty string, if the comand
--  string is empty.
getName :: (Functor m, MonadThrow m) => Text -> m Text
getName = fromMaybe mempty . L.head <$=< readArgs

-- |Surrounds an argument in quote marks, if necessary.
--  This is useful when arguments were extracted via 'readArgs', which deletes
--  quote marks. Quotes are placed around the input iff it is empty or contains
--  whitespace.
quoteArg :: Text -> Text
quoteArg x = if T.null x || T.any isSpace x then '\"' `T.cons` x `T.snoc` '\"'
                                            else x


-- |Creates a command without parameters.
makeCommand :: (MonadIO m, MonadCatch m,
                Functor m, Monoid i)
            => Text -- ^Command name.
            -> (i -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> (i -> m z)
               -- ^Command function. It will receive the first part of the input
               --  (customarily the command name), or the empty string if the
               --  input only contained whitespace.
            -> Command m i z
makeCommand n t d f = Command n t d f'
   where
      f' args = do res <- f $ fromMaybe mempty $ L.head args
                   return (res, L.drop 1 args)


-- |Creates a command with one parameter.
makeCommand1 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
                     -- ^If True, running the command will run the Asker's
                     --  IO action if not enough input is provided. If False
                     --  a 'ParamNumError' will be thrown.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> (Text -> a -> m z) -- ^Command function.
             -> Command m Text z
makeCommand1 n t d canAsk p1 f = Command n t d f'
   where
      mx = 1
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   res <- f x0 x1
                   return (res, L.drop (mx+1) args)

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> (Text -> a -> b -> m z) -- ^Command function.
             -> Command m Text z
makeCommand2 n t d canAsk p1 p2 f = Command n t d f'
   where
      mx = 2
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   res <- f x0 x1 x2
                   return (res, L.drop (mx+1) args)

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> (Text -> a -> b -> c -> m z) -- ^Command function.
             -> Command m Text z
makeCommand3 n t d canAsk p1 p2 p3 f = Command n t d f'
   where
      mx = 3
      f' args = do let x0 = fromMaybe "" $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   res <- f x0 x1 x2 x3
                   return (res, L.drop (mx+1) args)

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> (Text -> a -> b -> c -> d -> m z) -- ^Command function.
             -> Command m Text z
makeCommand4 n t d canAsk p1 p2 p3 p4 f = Command n t d f'
   where
      mx = 4
      f' args = do let x0 = fromMaybe "" $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   x4 <- askC canAsk p4 args mx 4
                   res <- f x0 x1 x2 x3 x4
                   return (res, L.drop (mx+1) args)

-- |Creates a command with five parameters.
makeCommand5 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> (Text -> a -> b -> c -> d -> e -> m z) -- ^Command function.
             -> Command m Text z
makeCommand5 n t d canAsk p1 p2 p3 p4 p5 f = Command n t d f'
   where
      mx = 5
      f' args = do let x0 = fromMaybe "" $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   x4 <- askC canAsk p4 args mx 4
                   x5 <- askC canAsk p5 args mx 5
                   res <- f x0 x1 x2 x3 x4 x5
                   return (res, L.drop (mx+1) args)

-- |Creates a command with six parameters.
makeCommand6 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> Asker m f -- ^'Asker' for the sixth parameter.
             -> (Text -> a -> b -> c -> d -> e -> f -> m z) -- ^Command function.
             -> Command m Text z
makeCommand6 n t d canAsk p1 p2 p3 p4 p5 p6 f = Command n t d f'
   where
      mx = 6
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   x4 <- askC canAsk p4 args mx 4
                   x5 <- askC canAsk p5 args mx 5
                   x6 <- askC canAsk p6 args mx 6
                   res <- f x0 x1 x2 x3 x4 x5 x6
                   return (res, L.drop (mx+1) args)

-- |Creates a command with seven parameters.
makeCommand7 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> Asker m f -- ^'Asker' for the sixth parameter.
             -> Asker m g -- ^'Asker' for the seventh parameter.
             -> (Text -> a -> b -> c -> d -> e -> f -> g -> m z) -- ^Command function.
             -> Command m Text z
makeCommand7 n t d canAsk p1 p2 p3 p4 p5 p6 p7 f = Command n t d f'
   where
      mx = 7
      f' args = do let x0 = fromMaybe "" $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   x4 <- askC canAsk p4 args mx 4
                   x5 <- askC canAsk p5 args mx 5
                   x6 <- askC canAsk p6 args mx 6
                   x7 <- askC canAsk p7 args mx 7
                   res <- f x0 x1 x2 x3 x4 x5 x6 x7
                   return (res, L.drop (mx+1) args)

-- |Creates a command with eight parameters.
makeCommand8 :: (MonadIO m, MonadCatch m, Functor m)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second parameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> Asker m e -- ^'Asker' for the fifth parameter.
             -> Asker m f -- ^'Asker' for the sixth parameter.
             -> Asker m g -- ^'Asker' for the seventh parameter.
             -> Asker m h -- ^'Asker' for the eighth parameter.
             -> (Text -> a -> b -> c -> d -> e -> f -> g -> h -> m z) -- ^Command function.
             -> Command m Text z
makeCommand8 n t d canAsk p1 p2 p3 p4 p5 p6 p7 p8 f = Command n t d f'
   where
      mx = 8
      f' args = do let x0 = fromMaybe "" $ L.head args
                   x1 <- askC canAsk p1 args mx 1
                   x2 <- askC canAsk p2 args mx 2
                   x3 <- askC canAsk p3 args mx 3
                   x4 <- askC canAsk p4 args mx 4
                   x5 <- askC canAsk p5 args mx 5
                   x6 <- askC canAsk p6 args mx 6
                   x7 <- askC canAsk p7 args mx 7
                   x8 <- askC canAsk p8 args mx 8
                   res <- f x0 x1 x2 x3 x4 x5 x6 x7 x8
                   return (res, L.drop (mx+1) args)


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
             -> Bool -- ^Whether the command can ask for input. This only
                     --  affects the necessary parameters.
             -> [Asker m a] -- ^'Asker's for the necessary parameters.
             -> [Asker m a] -- ^'Asker's for the optional parameters.
             -> (Text -> [a] -> m z)
             -> Command m Text z
makeCommandN n t d canAsk necc opt f = Command n t d f'
   where
      min = P.length necc
      max = natLength necc + natLength opt

      f' args = do neccParams <- unfoldrM (comb args) (necc,1, Nothing)
                   let x0 = maybe "" id (L.head args)
                       from = L.length neccParams + 1
                       to = Just $ L.length args - 1
                   optParams <- unfoldrM (comb args) (opt, from, to)
                   let params = neccParams L.++ optParams
                   res <- f x0 params
                   return (res, L.drop (length params + 1) args)

      -- |Goes through the list of askers until all are done or until the first
      --  AskFailure occurs. The results are of type @Either (AskFailure e) z@,
      --  the state is of type @([Asker m a e], Int)@. The second component @i@
      --  indicates that the @i@th parameter is to be read.
      comb _ ([],_,_) = return Nothing
      comb inp (x:xs, i, j) =
         if isJust j && fromJust j < i then return Nothing
         else askC canAsk x inp min i >$> args xs >$> Just

         where args ys y = (y,(ys,i+1,j))

      askC True f xs _ i = ask f (xs L.!! i)
      askC False f xs j i = maybe (throwM $ TooFewParamsError j (length xs - 1)) (ask f . Just) (xs L.!! i)

-- |Prints out a list of command names, with their descriptions.
summarizeCommands :: MonadIO m
                  => [Command m2 i z]
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

askC :: (MonadIO m, MonadCatch m, Functor m)
     => Bool -> Asker m a -> [Text] -> Int -> Int -> m a
askC True f xs _ i = ask f (xs L.!! i)
askC False f xs j i = maybe (throwM $ TooFewParamsError j (length xs - 1)) (ask f . Just) (xs L.!! i)


-- |Runs a REPL based on a set of commands.
--  For a line of input, the commands are tried in following order:
--
--  * the "exit" command,
--  * all regular commands, and then
--  * the "unknown" command.
makeREPL :: (Functor m, MonadIO m, MonadCatch m, Functor f, Foldable f)
         => [Command m Text a]
            -- ^The regular commands.
         -> Command m Text b
            -- ^The "exit" command which terminates the loop.
         -> Command m Text c
            -- ^The command that is called when none of the others match.
            --  This one's 'commandTest' is replaced with @const True@.
         -> m Text
            -- ^The asker to execute before each command (i.e. the prompt).
         -> f (Handler m ())
            -- ^Handlers for any exceptions that may arise. Generally, you
            --  will want to handle at least the exceptions of this module
            --  ('SomeCommandError', 'MalformedParamsError', 'TooManyParamsError',
            --   'TooFewParamsError'), and whatever the 'Asker' can throw.
         -> m ()
            -- ^Asks the user repeatedly for input, until the input matches
            --  the command test of the "exit" command.
makeREPL regular exit unknown prompt handlers = void $ iterateUntil id iter
   where
      iter = (prompt >>= runSingleCommand allCommands)
             `catches` handlers'

      handlers' = fmap (\(Handler f) -> Handler (\e -> f e >> return False)) handlers
      exit' = fmap (const True) exit
      regular' = L.map (fmap (const False)) regular
      unknown' = fmap (const False) $ unknown{commandTest = const True}

      allCommands = oneOf "" "" (exit' : regular' ++ [unknown'])
