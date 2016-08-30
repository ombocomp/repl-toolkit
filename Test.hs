module Main where

import qualified Data.Text as T

import System.REPL

main :: IO ()
main = makeREPLSimple [cmdNone,
                       cmdOne,
                       cmdTwo,
                       cmdN,
                       cmdN2,
                       cmdN3,
                       cmdOne',
                       cmdTwo',
                       cmdN',
                       cmdN2',
                       cmdN3']

cmdNone :: Command IO T.Text ()
cmdNone = makeCommand
             "cmdNoParams"
             (defCommandTest ["cmdNone"])
             ("a command with no parameters")
             (\t -> putStrLn "cmdNone OK")

cmdOne :: Command IO T.Text ()
cmdOne = makeCommand1
             "cmdOne"
             (defCommandTest ["cmdOne"])
             ("a command with one parameter")
             False
             lineAsker
             (\t x ->
               putStrLn $ "cmdOne OK; params:"
                          ++ T.unpack x)

cmdTwo :: Command IO T.Text ()
cmdTwo = makeCommand2
             "cmdTwo"
             (defCommandTest ["cmdTwo"])
             ("a command with two parameters")
             False
             lineAsker
             lineAsker
             (\t x y ->
               putStrLn $ "cmdTwo OK; params:"
                          ++ T.unpack x ++ ", "
                          ++ T.unpack y)

cmdN :: Command IO T.Text ()
cmdN = makeCommandN
             "cmdN"
             (defCommandTest ["cmdN"])
             ("a commandN with three parameters")
             False
             [lineAsker, lineAsker, lineAsker]
             []
             (\t xs ->
               putStrLn $ "cmdN OK; params:"
                          ++ concat (map T.unpack xs))

cmdN2 :: Command IO T.Text ()
cmdN2 = makeCommandN
             "cmdN2"
             (defCommandTest ["cmdN2"])
             ("a commandN with 3-4 parameters")
             False
             [lineAsker, lineAsker, lineAsker]
             [lineAsker]
             (\t xs ->
               putStrLn $ "cmdN2 OK; params:"
                          ++ concat (map T.unpack xs))

cmdN3 :: Command IO T.Text ()
cmdN3 = makeCommandN
             "cmdN3"
             (defCommandTest ["cmdN3"])
             ("a commandN with >=3 parameters")
             False
             [lineAsker, lineAsker, lineAsker]
             (repeat lineAsker)
             (\t xs ->
               putStrLn $ "cmdN3 OK; params:"
                          ++ concat (map T.unpack xs))

cmdOne' :: Command IO T.Text ()
cmdOne' = makeCommand1
             "cmdOne'"
             (defCommandTest ["cmdOne'"])
             ("a command with one parameter (interactive)")
             True
             lineAsker
             (\t x ->
               putStrLn $ "cmdTwo' OK; params:"
                          ++ T.unpack x)

cmdTwo' :: Command IO T.Text ()
cmdTwo' = makeCommand2
             "cmdTwo'"
             (defCommandTest ["cmdTwo'"])
             ("a command with two parameters (interactive)")
             True
             lineAsker
             lineAsker
             (\t x y ->
               putStrLn $ "cmdTwo' OK; params:"
                          ++ T.unpack x ++ ", "
                          ++ T.unpack y)

cmdN' :: Command IO T.Text ()
cmdN' = makeCommandN
             "cmdN'"
             (defCommandTest ["cmdN'"])
             ("a commandN with three parameters (interactive)")
             True
             [lineAsker, lineAsker, lineAsker]
             []
             (\t xs ->
               putStrLn $ "cmdN' OK; params:"
                          ++ concat (map T.unpack xs))

cmdN2' :: Command IO T.Text ()
cmdN2' = makeCommandN
             "cmdN2'"
             (defCommandTest ["cmdN2'"])
             ("a commandN with 3-4 parameters (interactive)")
             True
             [lineAsker, lineAsker, lineAsker]
             [lineAsker]
             (\t xs ->
               putStrLn $ "cmdN2' OK; params:"
                          ++ concat (map T.unpack xs))

cmdN3' :: Command IO T.Text ()
cmdN3' = makeCommandN
             "cmdN3'"
             (defCommandTest ["cmdN3'"])
             ("a commandN with >=3 parameters (interactive)")
             True
             [lineAsker, lineAsker, lineAsker]
             (repeat lineAsker)
             (\t xs ->
               putStrLn $ "cmdN3' OK; params:"
                          ++ concat (map T.unpack xs))
