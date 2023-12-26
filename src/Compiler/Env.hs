module Compiler.Env where

import Compiler.TAMmt
import Compiler.ASTmt

type VarEnv = [(Identifier,StkAddress)] -- [(String,Int)]

{- You may want to define this in its own module together
   with some auxiliary functions; here, we just define the
  bare minimum that helps us get going! -}

address :: VarEnv -> Identifier -> StkAddress
address ve v = case lookup v ve of
                 Nothing -> error "Variable not in environment"
                 Just a -> a

