module Compiler.State where

import Control.Monad
import Control.Applicative

newtype ST state val = S (state -> (val,state))
-- e.g. ST Int [TAMInst]

{- It would be nice if we could just write:
   type ST state val = state -> (val,state)
   (but this wouldn't allow us to make it an
   instance of Functor/Application/Monad later)
   Instead, we define: -}
app :: ST state a -> state -> (a,state)
app (S f) x = f x

-- Auxiliary functions
stUpdate :: state -> ST state ()
stUpdate st = S (\s -> ((), st))

stState :: ST state state
stState = S (\s -> (s,s))



{- Note:
   examples of type constructors:
   List ([]), Maybe, Tree
     (these take a type as argument and construct a new type)
   Note that "ST state" is also such a type constructor, as it
   takes one more type (the type of values) as argument!
-}

instance Functor (ST state) where
  -- fmap :: (a -> b) -> (ST state a -> ST state b)
  fmap f sta = S (\s -> let (x,s') = app sta s in (f x, s'))

instance Applicative (ST state) where
  -- pure :: a -> ST state a
  pure x = S (\s -> (x,s))
  -- (<*>) :: ST state (a -> b) -> ST state a -> ST state b
  stf <*> sta = S (\s ->
                     let (f,s') = app stf s
                         (x,s'') = app sta s'
                     in (f x , s''))

instance Monad (ST state) where
  -- (>>=) :: ST state a -> (a -> ST state b) -> ST state b
  sta >>= f = S (\s ->
                   let (x,s') = app sta s
                       stb = f x
                       (y,s'') = app stb s'
                   in (y,s''))
