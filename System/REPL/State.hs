-- |Helper functions relating to State.
module System.REPL.State (
   -- *Convenience functions for handling state
   -- |These can be convenient when one wishes to
   --  to extract a number of elements from the current state via pattern
   --  -matching, e.g.
   --
   --  @ data State = State{f::a,g::b,h::c}
   --    ...
   --    do (x,z) <- get2 f h
   --       ...do something with x and z... @
   get1,
   get2,
   get3,
   get4,
   get5,
   get6,
   get7,
   get8,
   )where

import Control.Arrow
import Control.Monad
import Control.Monad.State

-- |Extracts a result from the current state.
--  Defined as @get1 f = liftM f get@.
get1 :: Monad m
     => (s -> a)
     -> StateT s m a
get1 f1 = liftM f1 get

-- |Extracts two results from the current state.
get2 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> StateT s m (a,b)
get2 f1 f2 = liftM (f1 &&& f2) get

-- |Extracts three results from the current state.
get3 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> StateT s m (a,b,c)
get3 f1 f2 f3 = liftM (\x -> (f1 x,f2 x, f3 x)) get

-- |Extracts four results from the current state.
get4 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> StateT s m (a,b,c,d)
get4 f1 f2 f3 f4 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x)) get

-- |Extracts five results from the current state.
get5 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> StateT s m (a,b,c,d,e)
get5 f1 f2 f3 f4 f5 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x)) get

-- |Extracts six results from the current state.
get6 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> StateT s m (a,b,c,d,e,f)
get6 f1 f2 f3 f4 f5 f6 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x)) get

-- |Extracts seven results from the current state.
get7 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> StateT s m (a,b,c,d,e,f,g)
get7 f1 f2 f3 f4 f5 f6 f7 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x)) get

-- |Extracts eight results from the current state.
get8 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> (s -> h)
     -> StateT s m (a,b,c,d,e,f,g,h)
get8 f1 f2 f3 f4 f5 f6 f7 f8 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x, f8 x)) get

