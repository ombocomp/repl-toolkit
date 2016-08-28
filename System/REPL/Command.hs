-- |Provides Commands for REPLs. Commands are there to provide high-level
--  handling of user input and to offer functionality in a standard, composable
--  way.
--
--  Whereas an 'Asker' is good for getting a single value, a 'Command' can get
--  multiple inputs and be composed with other commands.
--
--  Use cases:
--
--  1. Getting specific numbers of arguments or optional arguments from the user. E.g.
--
--     @
--     \{\-\# LANGUAGE OverloadedStrings \#\-\}
--
--     import Data.Text (unpack)
--
--     asker :: Asker' IO String
--     asker = Asker "Enter argument: " (Right . unpack) (return . Right)
--
--     cmd = makeCommand3 "command" ("command"==) "description" True [asker,asker,asker] (\t x y z -> putStrLn "yay!")
--     @
--
--     This is a command with 3 arguments. The user can enter the arguments
--     in the same line or give them one by one:
--
--     >>> command arg1 arg2 arg3
--     yay!
--
--     >>> command
--     Enter argument:
--     >>> arg1
--     Enter  argument:
--     >>> arg2
--     Enter argument:
--     >>> arg3
--     yay!
--
--     Had we set the bool above to @False@, only the first form would have been allowed.
--
--     Arguments can contain whitespace if they are surrounded with quotes:
--
--     >>> command "arg1 with spaces" arg2 arg3
--     yay!
--
--     Optional arguments are also possible:
--
--     @
--     cmd = makeCommandN "command" ("command"==) "description" True [asker] [optAsker]
--                        (\t (x:xs) -> do putStrLn ("Required argument: " ++ x)
--                                         if null xs then putStrLn "No optional argument."
--                                         else putStrLn ("Optional argument: " ++ head xs))
--     @
--
--     >>> command arg1
--     Required argument: arg1
--
--     >>> command arg1 arg2
--     Required argument: arg1
--     Optional argument: arg2
--
--  2. Creating command hierarchies, e.g.
--
--     @
--     commit = makeCommand 1 "commit" ...
--     sendEmail = makeCommand "send-email"
--     sendTweet = makeCommand "send-tweet"
--
--     commit' = subcommand commit [sendEmail, sendTweet]
--
--     main = makeREPLSimple [commit']
--     @
--
--     >>> myVersionControl commit "my first commit" send-email
--
--     Here, @commit@ is the root command and @sendEmail@, @sendTweet@ its two
--     possible sub-commands. The sub-commands get executed after their root command.
--
--  3. Making a REPL out of some commands.
--
--     As above, one can use 'makeREPL' or 'makeREPLSimple' to create a
--     REPL out of a list of commands and use it as the @main@ function instead
--     of going through the chore of writing a loop it by hand.
module System.REPL.Command (
   -- *Command class
   Command(..),
   oneOf,
   subcommand,
   -- **Running commands
   -- |You can use 'runPartialCommand' to run a command as well, but one generally doesn't want left-over input.
   runCommand,
   runSingleCommand,
   runSingleCommandIf,
   -- **Making REPLs
   makeREPL,
   makeREPLSimple,
   -- *Exceptions
   -- |These are the exceptions that can be thrown during the course of command
   --  invocation (in addition to those that you throw yourself, of course).
   --
   --  SomeCommandError is an abstract exception and all others are its concrete
   --  subclasses. See the example in "Control.Exception" for details.
   SomeREPLError(..),
   SomeCommandError(..),
   MalformedParamsError(..),
   TooFewParamsError(..),
   TooManyParamsError(..),
   -- * Dealing with arguments
   readArgs,
   getName,
   defCommandTest,
   quoteArg,
   -- * Helpers
   summarizeCommands,
   -- * Making commands
   -- |Ignore the "a0"-type parameters in the Askers.
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
   -- * Example commands.
   -- |A few commands for convenience.
   noOpCmd,
   defExitCmd,
   defHelpCmd,
   defErrorHandler,
   ) where

import Prelude hiding (putStrLn, putStr, (++), length, replicate)
import qualified Prelude as P

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops (unfoldrM, iterateUntil)
import Data.Char (isSpace)
import qualified Data.Functor.Bind as Bi
import Data.Functor.Monadic
import qualified Data.List as LU
import qualified Data.List.Safe as L
import Data.ListLike(ListLike(..))
import Data.ListLike.IO (ListLikeIO(..))
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Ord
import Data.Typeable (cast)
import qualified Data.Text as T
import System.REPL.Ask
import System.REPL.Types
import qualified System.REPL.Prompt as PR
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

-- alias for Data.ListLike.append
(++) :: (ListLike full item) => full -> full -> full
(++) = append

-- |Runs the command with the input text as parameter, discarding any left-over
--  input. The command test is disregarded.
--
--  Can throw:
--
--  * 'MalformedParamsError'
runCommand :: (MonadThrow m) => Command m T.Text a -> T.Text -> m a
runCommand c = fmap fst . runPartialCommand c <=< readArgs

-- |Runs the command with the input text as parameter.
--  The command test is disregarded.
--
--  Can throw:
--
--  * 'MalformedParamsError'
--  * 'TooManyParamsError', if any input is left unconsumed.
--
--  __Note:__ 'TooManyParamsError' will only be thrown after the command's execution
--  is attempted. This is because of the subcommand mechanism, which prevents the
--  static determination of the number of required arguments.
runSingleCommand :: (MonadThrow m) => Command m T.Text a -> T.Text -> m a
runSingleCommand c t = fromJust <$> runSingleCommandIf (c{commandTest = const True}) t

-- |Runs the command with the input text as parameter.
--
--  The first parameter (or the empty string, if no input was given)
--  is passed to the command test. If it fails the test, 'Nothing' is returned.
--
--  Can throw:
--
--  * 'MalformedParamsError'
--  * 'TooManyParamsError', if any input is left unconsumed.
runSingleCommandIf :: MonadThrow m => Command m T.Text a -> T.Text -> m (Maybe a)
runSingleCommandIf c t = do
   t' <- readArgs t
   let t'' = if L.null t' then "" else LU.head t'
   if not (commandTest c t'') then return Nothing
   else do
      (res, output) <- runPartialCommand c t'
      let act = length t'
          mx  = act - length output
      when (not . L.null $ output) (throwM $ TooManyParamsError mx act)
      return $ Just res


-- |Takes a list @xs@ and executes the first command in a list whose
--  'commandTest' matches the input.
--
--  Note that the resultant command @c@'s' 'runPartialCommand' should only be
--  executed with an input @t@ if 'commandTest c t' == True', where @t'@ is either
--  @head (readArgs t)@ or @mempty@ if @t@ is empty.
--  Otherwise, the result is undefined.
oneOf :: Monoid i
      => T.Text
         -- ^Command name.
      -> T.Text
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
subcommand :: (Monad m, Monoid i)
           => Command m i a
              -- ^The root command.
           -> [a -> Command m i b]
              -- ^The subcommands that may follow it. This list must be finite.
           -> Command m i b
subcommand x xs = x Bi.>>- \y -> oneOf "" "" (L.map ($ y) xs)

-- |Splits and trims the input of a command. If the input cannot be parsed, a
--  'MalformedParamsError' exception is thrown.
--
--  === Format
--
--  Any non-whitespace sequence of characters is interpreted as
--  one argument, unless double quotes (") are used, in which case
--  they demarcate an argument. Each argument is parsed as a haskell
--  string literal (quote-less arguments have quotes inserted around them).
--
--  Arguments are parsed using parsec's @stringLiteral@ (haskell-style),
--  meaning that escape sequences and unicode characters are handled automatically.
readArgs :: MonadThrow m => T.Text -> m [T.Text]
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

-- |Gets the first part of a command string. Returns Nothing
--  if the string is empty or if 'readArgs' throws a 'MalformedParamsError'.
getName :: T.Text -> Maybe T.Text
getName = readArgs >=> L.head

-- |The "default" command test for making commands.
--  This function uses 'getName' to extract the first part of the user input,
--  stripping whitespace and also checking whether the entire input is well-formed.
defCommandTest :: [T.Text] -- ^Command names, including permissible aliases.
               -> T.Text -- ^User input.
               -> Bool
defCommandTest xs = maybe False (`L.elem` xs) . getName

-- |Surrounds an argument in quote marks, if necessary.
--  This is useful when arguments were extracted via 'readArgs', which deletes
--  quote marks. Quotes are placed around the input iff it is empty or contains
--  whitespace.
quoteArg :: T.Text -> T.Text
quoteArg x = if T.null x || T.any isSpace x then '\"' `T.cons` x `T.snoc` '\"'
                                            else x

-- |Creates a command without parameters.
makeCommand :: (MonadIO m, MonadCatch m, Monoid i)
            => T.Text -- ^Command name.
            -> (i -> Bool) -- ^Command test.
            -> T.Text -- ^Command description.
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
makeCommand1 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
                     --  If True, running the command will run the Asker's
                     --  IO action if not enough input is provided. If False
                     --  a 'TooFewParamsError' will be thrown.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> (T.Text -> a -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand1 n t d canAsk p1 f = Command n t d f'
   where
      mx = 1
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   res <- f x0 x1
                   return (res, L.drop (mx+1) args)

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> (T.Text -> a -> b -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand2 n t d canAsk p1 p2 f = Command n t d f'
   where
      mx = 2
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   res <- f x0 x1 x2
                   return (res, L.drop (mx+1) args)

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> (T.Text -> a -> b -> c -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand3 n t d canAsk p1 p2 p3 f = Command n t d f'
   where
      mx = 3
      f' args = do let x0 = fromMaybe "" $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   res <- f x0 x1 x2 x3
                   return (res, L.drop (mx+1) args)

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> Asker m d0 d -- ^'Asker' for the fourth parameter.
             -> (T.Text -> a -> b -> c -> d -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand4 n t d canAsk p1 p2 p3 p4 f = Command n t d f'
   where
      mx = 4
      f' args = do let x0 = fromMaybe "" $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   x4 <- askC p4 args 4
                   res <- f x0 x1 x2 x3 x4
                   return (res, L.drop (mx+1) args)

-- |Creates a command with five parameters.
makeCommand5 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> Asker m d0 d -- ^'Asker' for the fourth parameter.
             -> Asker m e0 e -- ^'Asker' for the fifth parameter.
             -> (T.Text -> a -> b -> c -> d -> e -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand5 n t d canAsk p1 p2 p3 p4 p5 f = Command n t d f'
   where
      mx = 5
      f' args = do let x0 = fromMaybe "" $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   x4 <- askC p4 args 4
                   x5 <- askC p5 args 5
                   res <- f x0 x1 x2 x3 x4 x5
                   return (res, L.drop (mx+1) args)

-- |Creates a command with six parameters.
makeCommand6 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> Asker m d0 d -- ^'Asker' for the fourth parameter.
             -> Asker m e0 e -- ^'Asker' for the fifth parameter.
             -> Asker m f0 f -- ^'Asker' for the sixth parameter.
             -> (T.Text -> a -> b -> c -> d -> e -> f -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand6 n t d canAsk p1 p2 p3 p4 p5 p6 f = Command n t d f'
   where
      mx = 6
      f' args = do let x0 = fromMaybe mempty $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   x4 <- askC p4 args 4
                   x5 <- askC p5 args 5
                   x6 <- askC p6 args 6
                   res <- f x0 x1 x2 x3 x4 x5 x6
                   return (res, L.drop (mx+1) args)

-- |Creates a command with seven parameters.
makeCommand7 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> Asker m d0 d -- ^'Asker' for the fourth parameter.
             -> Asker m e0 e -- ^'Asker' for the fifth parameter.
             -> Asker m f0 f -- ^'Asker' for the sixth parameter.
             -> Asker m g0 g -- ^'Asker' for the seventh parameter.
             -> (T.Text -> a -> b -> c -> d -> e -> f -> g -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand7 n t d canAsk p1 p2 p3 p4 p5 p6 p7 f = Command n t d f'
   where
      mx = 7
      f' args = do let x0 = fromMaybe "" $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   x4 <- askC p4 args 4
                   x5 <- askC p5 args 5
                   x6 <- askC p6 args 6
                   x7 <- askC p7 args 7
                   res <- f x0 x1 x2 x3 x4 x5 x6 x7
                   return (res, L.drop (mx+1) args)

-- |Creates a command with eight parameters.
makeCommand8 :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input.
             -> Asker m a0 a -- ^'Asker' for the first parameter.
             -> Asker m b0 b -- ^'Asker' for the second parameter.
             -> Asker m c0 c -- ^'Asker' for the third parameter.
             -> Asker m d0 d -- ^'Asker' for the fourth parameter.
             -> Asker m e0 e -- ^'Asker' for the fifth parameter.
             -> Asker m f0 f -- ^'Asker' for the sixth parameter.
             -> Asker m g0 g -- ^'Asker' for the seventh parameter.
             -> Asker m h0 h -- ^'Asker' for the eighth parameter.
             -> (T.Text -> a -> b -> c -> d -> e -> f -> g -> h -> m z) -- ^Command function.
             -> Command m T.Text z
makeCommand8 n t d canAsk p1 p2 p3 p4 p5 p6 p7 p8 f = Command n t d f'
   where
      mx = 8
      f' args = do let x0 = fromMaybe "" $ L.head args
                   when (not canAsk) $ checkParamNum args mx
                   x1 <- askC p1 args 1
                   x2 <- askC p2 args 2
                   x3 <- askC p3 args 3
                   x4 <- askC p4 args 4
                   x5 <- askC p5 args 5
                   x6 <- askC p6 args 6
                   x7 <- askC p7 args 7
                   x8 <- askC p8 args 8
                   res <- f x0 x1 x2 x3 x4 x5 x6 x7 x8
                   return (res, L.drop (mx+1) args)


-- |Creates a command with a list of parameters.
--  The first list @necc@ of 'Asker's indicates the necessary parameters;
--  the user must at least provide this many. The second list @opt@ contains
--  'Asker's for additional, optional parameters, and may be infinite.
--  If the number of passed parameters exceeds
--  @length necc + length opt@, or if any 'Asker' fails,
--  the command returns an 'AskFailure'.
makeCommandN :: (MonadIO m, MonadCatch m)
             => T.Text -- ^Command name.
             -> (T.Text -> Bool) -- ^Command test.
             -> T.Text -- ^Command description
             -> Bool -- ^Whether the command can ask for input. This only
                     --  affects the necessary parameters.
             -> [Asker m a0 a] -- ^'Asker's for the necessary parameters.
             -> [Asker m b0 a] -- ^'Asker's for the optional parameters.
             -> (T.Text -> [a] -> m z)
             -> Command m T.Text z
makeCommandN n t d canAsk necc opt f = Command n t d f'
   where
      min = P.length necc

      f' args = do when (not canAsk) $ checkParamNum args min
                   neccParams <- unfoldrM (comb args) (necc,1, Nothing)
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
      comb inp (x:xs, i, j) = if isJust j && fromJust j < i
                              then return Nothing
                              else askC x inp i >$> args xs >$> Just
         where
            args ys y = (y,(ys,i+1,j))

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

-- |Throws a 'TooFewParamsError' if the length of the list is smaller than the second argument.
checkParamNum :: MonadThrow m => [a] -> Int -> m ()
checkParamNum xs need = if have < need then throwM $ TooFewParamsError need have else return ()
   where have = length xs - 1

-- |Wrapper for 'ask'.
askC :: (MonadIO m, MonadCatch m)
     => Asker m a0 a -> [T.Text] -> Int -> m a
askC f xs i = ask f (xs L.!! i)

--askC False f xs j i = maybe (throwM $ TooFewParamsError j (length xs - 1)) (ask f . Just) (xs L.!! i)

-- |Runs a REPL based on a set of commands.
--  For a line of input, the commands are tried in following order:
--
--  * the "exit" command,
--  * all regular commands, and then
--  * the "unknown" command.
makeREPL :: (MonadIO m, MonadCatch m)
         => [Command m T.Text a]
            -- ^The regular commands.
         -> Command m T.Text b
            -- ^The "exit" command which terminates the loop.
         -> Command m T.Text c
            -- ^The command that is called when none of the others match.
            --  This one's 'commandTest' is replaced with @const True@.
         -> m T.Text
            -- ^The asker to execute before each command (i.e. the prompt).
         -> [Handler m ()]
            -- ^List of Handlers for any exceptions that may arise.
            --  The exception hierchy is rooted in 'SomeREPLError'.
            --  See "System.REPL.Types".
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

-- |A variant of 'makeREPL' with some default settings:
--
--  * The "exit" command is 'defExitCmd'.
--  * The "unknown" command prints "Unknown command: <user input>".
--  * The prompt is "> ".
--  * The error handler is 'defErrorHandler'.
makeREPLSimple :: (MonadIO m, MonadCatch m)
               => [Command m T.Text a]
               -> m ()
makeREPLSimple regular = makeREPL regular defExitCmd unknownCmd PR.prompt defErrorHandler
   where
      unknownCmd = makeCommandN "" (const True) "" False [] (repeat lineAsker)
                                (\t _ -> liftIO $ PR.putStrLn $ "Unknown command: " ++ t)

-- Example commands
-------------------------------------------------------------------------------

-- |A command that takes no arguments and does nothing.
noOpCmd :: (MonadIO m, MonadCatch m)
        => T.Text
           -- ^Command name.
        -> [T.Text]
           -- ^Alternative names for the command. The user can either
           --  the command name or any of the alternative names.
           --
           --  E.g. "exit" with alternative names ":e", ":quit".
        -> Command m T.Text ()
noOpCmd n ns = makeCommand n ((`L.elem` (n:ns)) . T.strip) "" (const $ return ())

-- |A command with the name ":exit" and the description
--  "Exits the program." Otherwise, it does nothing.
--
--  You can use this as the exit-command for 'makeREPL',
--  if no special clean-up is needed upon quitting.
defExitCmd :: (MonadIO m, MonadCatch m)
           => Command m T.Text ()
defExitCmd = makeCommand n ((n==) . T.strip) "Exits the program." (const $ return ())
   where
      n = ":exit"

-- |A help-command with the name ":help" and the
--  description "Prints this help text."
--
--  It goes through the given list of commands and prints
--  the name and description of each one.
defHelpCmd :: (MonadIO m, MonadCatch m, Foldable f)
           => f (Command m0 a b)
           -> Command m T.Text ()
defHelpCmd cmds = makeCommand n ((n==) . T.strip) "Prints this help text." help
   where
      n = ":help"
      help _ = liftIO $ mapM_ (\x -> putStrLn $ commandName x ++ " - " ++ commandDesc x) cmds

-- |A default error handler that catches 'SomeREPLError' and prints it to stdout.
--
--  For the following errors, we print a user-friendly error message:
--
-- * 'GenericTypeError' (when wrapped in an 'AskerTypeError'),
-- * 'GenericPredicateError' (when wrapped in an 'AskerPredicateError'),
-- * 'PathRootDoesNotExist' (when wrapped in an 'AskerPredicateError'),
-- * 'PathIsNotWritable' (when wrapped in an 'AskerPredicateError'),
-- * 'GenericPredicateError' (when wrapped in an 'AskerPredicateError'),
-- * 'AskerInputAbortedError',
-- * 'MalformedParamsError',
-- * 'TooManyParamsError',
-- * 'TooFewParamsError',
-- * 'NoConfigFileParseError'.
--
--  For every other subtype of 'SomeREPLError', we just print the Show-instance.
--
--  Useful in combination with 'makeREPL'.
defErrorHandler :: MonadIO m
                => [Handler m ()]
defErrorHandler =
   [Handler h_askerGenericTypeError,
    Handler h_askerGenericPredicateError,
    Handler h_askerPathRootDoesNotExist,
    Handler h_askerPathIsNotWritable,
    Handler h_tooMalformedParamsError,
    Handler h_tooManyParamsError,
    Handler h_tooFewParamsError,
    Handler h_noConfigFileParseError,
    Handler h]
   where
      put :: String -> IO ()
      put = putStrLn

      h :: MonadIO m => SomeREPLError -> m ()
      h = liftIO . print

      h_askerGenericTypeError :: MonadIO m => AskerTypeError -> m ()
      h_askerGenericTypeError (AskerTypeError e) = case fromException e of
         Just (GenericTypeError t) -> liftIO . put . T.unpack $ t
         Nothing -> liftIO . print $ e


      h_askerGenericPredicateError :: MonadIO m => AskerPredicateError -> m ()
      h_askerGenericPredicateError (AskerPredicateError e) = case fromException e of
         Just (GenericPredicateError t) -> liftIO . put . T.unpack $ t
         Nothing -> liftIO . print $ e

      h_askerPathRootDoesNotExist :: MonadIO m => AskerPredicateError -> m ()
      h_askerPathRootDoesNotExist (AskerPredicateError e) = case fromException e of
         Just (PathRootDoesNotExist fp) -> liftIO $ put $
                                           "The root of the path '" ++ fp ++
                                           "' does not exist."
         Nothing -> liftIO . print $ e

      h_askerPathIsNotWritable :: MonadIO m => AskerPredicateError -> m ()
      h_askerPathIsNotWritable (AskerPredicateError e) = case fromException e of
         Just (PathIsNotWritable fp) -> liftIO $ put $
                                           "The path '" ++ fp ++
                                           "' is not writable."
         Nothing -> liftIO . print $ e

      h_askerInputAbortedError :: MonadIO m => AskerTypeError -> m ()
      h_askerInputAbortedError (AskerTypeError e) =
         liftIO $ put "Input aborted."

      h_tooMalformedParamsError :: MonadIO m => MalformedParamsError -> m ()
      h_tooMalformedParamsError (MalformedParamsError t) = liftIO . put $
         "Error parsing parameters: " ++ T.unpack t

      h_tooManyParamsError :: MonadIO m => TooManyParamsError -> m ()
      h_tooManyParamsError (TooManyParamsError m x) = liftIO . put $
         "Expected at most " ++ show m ++ " parameters, got " ++ show x ++ "."

      h_tooFewParamsError :: MonadIO m => TooFewParamsError -> m ()
      h_tooFewParamsError (TooFewParamsError m x) = liftIO . put $
         "Expected at least " ++ show m ++ " parameters, got " ++ show x ++ "."

      h_noConfigFileParseError :: MonadIO m => NoConfigFileParseError -> m ()
      h_noConfigFileParseError (NoConfigFileParseError t) = liftIO . put $
         "Error parsing configuration file: " ++ T.unpack t
