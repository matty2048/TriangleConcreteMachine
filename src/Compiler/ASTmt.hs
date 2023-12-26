module Compiler.ASTmt where

{- Grammar rules:

program ::= 'let' declarations 'in' command

declaration ::= 'var' identifier | 'var' identifier ':=' expr

declarations ::= declaration | declaration ; declarations | epsilon
                 (just use lists)

command ::= identifier ':=' expr
          | 'if' expr 'then' command 'else' command
          | 'while' expr 'do' command
          | 'getint' '(' identifier ')'
          | 'printint' '(' expr ')'
          | 'begin' commands 'end'

commands ::= command | command ';' commands | epsilon
                 (just use lists)

-}

data Program = LetIn [Declaration] Command
  deriving (Eq,Show)

data Declaration = VarDecl Identifier | VarInit Identifier Expr
  deriving (Eq,Show)

data Command = Assignment Identifier Expr
             | IfThenElse Expr Command Command
             | While Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
  deriving (Eq,Show)

type Identifier = String

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data Expr = LitInteger Int | Var Identifier
          | BinOp BinOperator Expr Expr
          | UnOp  UnOperator Expr
          | Conditional Expr Expr Expr
  deriving (Eq,Show)
