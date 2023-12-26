module Compiler.ParseProgram where

import           Control.Applicative
import           Data.Char
import           Compiler.FunParser
import           Compiler.ASTmt
import           Compiler.ExpParser



declAST :: Parser Declaration
declAST =do symbol "var"
            (do sym <- identifier
                symbol ":="
                exp <- expAST
                return $ VarInit sym exp
             <|>
             do sym <- identifier
                return $ VarDecl sym)



declsAST :: Parser [Declaration]
declsAST = do declsAST' []

commdelimAST :: Parser Command
commdelimAST = do com <- commAST
                  symbol ";"
                  return com


declsAST' :: [Declaration] -> Parser [Declaration]
declsAST' c =
   do n <- declAST
      do symbol ";"
         declsAST' $ c++[n]
       <|>
       do symbol "in"
          return $ c++[n]

commsAST' :: [Command] -> Parser [Command]
commsAST' c = do
   n <- commAST
   (do symbol ";"
       commsAST' (c++[n])
    <|>
    do
      symbol "end"
      return $ c++[n])

commsAST :: Parser [Command]
commsAST = commsAST' []

commAST :: Parser Command
commAST = do id <- identifier
             symbol ":="
             expr <- expAST
             return $ Assignment id expr
          <|>
          do symbol "if"
             expr <- expAST
             symbol "then"
             com1 <- commAST
             symbol "else"
             com2 <- commAST
             return $ IfThenElse expr com1 com2
          <|>
          do symbol "while"
             expr <- expAST
             symbol "do"
             com <- commAST
             return $ While expr com
          <|>
          do symbol "getint"
             symbol "("
             id <- identifier
             symbol ")"
             return $ GetInt id
          <|>
          do symbol "printint"
             symbol "("
             expr <- expAST
             symbol ")"
             return $ PrintInt expr
          <|>
          do symbol "begin"
             comms <- commsAST
             return $ BeginEnd comms


progAST :: Parser Program
progAST = do symbol "let"
             decls <- declsAST
             com <- commAST
             return $ LetIn decls com
