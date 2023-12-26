module Compiler.ExpParser where

import           Control.Applicative
import           Data.Char
import           Compiler.FunParser
import           Compiler.ASTmt


{-
  minExp := multExp - minExp | multExp + minExp | multExp
  multExp := divExp * multExp | divExp
  divExp := term/divExp | term
  term := int | (minExp) | -term
-}

symbolExpAST :: Parser Expr
symbolExpAST = do sym <- ident
                  return (Var sym)
               <|>
               do lit <- integer
                  return (LitInteger lit)


termExpAST' :: (Expr -> Expr) -> Parser Expr
termExpAST' f = do n <- symbolExpAST
                   (return (f n))
                <|>
                do symbol "-"
                   termExpAST' (\x -> UnOp Negation (f x))
                <|>
                do symbol "!"
                   termExpAST' (\x -> UnOp NegBool (f x))
                <|>
                do symbol "("
                   (do n <- expAST
                       (do symbol ")"
                           (return (f n))))

ternOpExpAst' :: (Expr -> Expr) -> Parser Expr
ternOpExpAst' f = do cond <- disjOpExpAst
                     (do symbol "?"
                         t <- ternOpExpAst
                         symbol ":"
                         fa <- ternOpExpAst
                         return (Conditional (f cond) t fa)
                      <|>
                      return (f cond))

disjOpExpAst' :: (Expr -> Expr) -> Parser Expr
disjOpExpAst' f = do n <- conjOpExpAst
                     (do symbol "||"
                         disjOpExpAst' (\x -> BinOp Disjunction (f n) x)
                         <|>
                         return (f n))

conjOpExpAst' :: (Expr -> Expr) -> Parser Expr
conjOpExpAst' f = do n <- condOpExpAst
                     (do symbol "&&"
                         conjOpExpAst' (\x -> BinOp Conjunction (f n) x)
                         <|>
                         return (f n))

condOpExpAst' :: (Expr -> Expr) -> Parser Expr
condOpExpAst' f = do n <- minExpAST
                     (do symbol "<"
                         condOpExpAst' (\x -> BinOp LssOp (f n) x)
                       <|>
                      do symbol "<="
                         condOpExpAst' (\x -> BinOp LeqOp (f n) x)
                       <|>
                      do symbol ">"
                         condOpExpAst' (\x -> BinOp GtrOp (f n) x)
                       <|>
                      do symbol ">="
                         condOpExpAst' (\x -> BinOp GeqOp (f n) x)
                       <|>
                      do symbol "=="
                         condOpExpAst' (\x -> BinOp EqOp (f n) x)
                       <|>
                      do symbol "!="
                         condOpExpAst' (\x -> BinOp NeqOp (f n) x)
                       <|>
                       return (f n))

minExpAST' :: (Expr -> Expr) -> Parser Expr
minExpAST' f = do n <- multExpAST
                  (do symbol "-"
                      minExpAST' (\x -> BinOp Subtraction (f n) x)
                    <|>
                   do symbol "+"
                      minExpAST' (\x -> BinOp Addition (f n) x)
                    <|>
                    return (f n))

multExpAST' :: (Expr -> Expr) -> Parser Expr
multExpAST' f = do n <- divExpAST
                   (do symbol "*"
                       multExpAST' (\x -> BinOp Multiplication (f n) x)
                    <|>
                       return (f n))

divExpAST' :: (Expr -> Expr) -> Parser Expr
divExpAST' f = do n <- termExpAST
                  (do symbol "/"
                      divExpAST' (\x -> BinOp Division (f n) x)
                    <|>
                      return (f n))

expAST' :: (Expr -> Expr) -> Parser Expr
expAST' f = do ternOpExpAst' f

ternOpExpAst :: Parser Expr
ternOpExpAst = ternOpExpAst' id

disjOpExpAst :: Parser Expr
disjOpExpAst = disjOpExpAst' id

conjOpExpAst :: Parser Expr
conjOpExpAst = conjOpExpAst' id

condOpExpAst :: Parser Expr
condOpExpAst = condOpExpAst' id

minExpAST :: Parser Expr
minExpAST = minExpAST' id

multExpAST :: Parser Expr
multExpAST = multExpAST' id

divExpAST :: Parser Expr
divExpAST = divExpAST' id

termExpAST :: Parser Expr
termExpAST = termExpAST' id


expAST :: Parser Expr
expAST = expAST' id
