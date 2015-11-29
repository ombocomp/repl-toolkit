-- |Contents:
--  
--  [/Ask/]
--     Asking the user for input in a principled way.
--     Reading, parsing errors, predicate checks are all handled.
--
--  [/Command/]
--     The main module of the package. Functions for creating
--     commands, which can receive and ask for arguments.
--     Commands are composable and can be built into a REPL.
--
--  [/Config/]
--     Read configuration files in various formats.
module System.REPL (
   module System.REPL.Ask,
   module System.REPL.Command,
   module System.REPL.Config,
   ) where

import System.REPL.Ask
import System.REPL.Command
import System.REPL.Config
