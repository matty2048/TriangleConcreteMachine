{-

Compilers Course (COMP3012), 2020
  Venanzio Capretta

Functional parsing library from chapter 13 of Programming in Haskell,
 Graham Hutton, Cambridge University Press, 2016.
 modified by Venanzio Capretta, 2020
-}

module Compiler.FunParser where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

-- Parsing only the complete input
parseAll :: Parser a -> String -> [a]
parseAll p s = [a | (a,"") <- parse p s]

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> map (\(v,out) -> (g v,out)) (parse p inp))

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> concat $
                          map (\(g,out) -> parse (fmap g px) out) (parse pg inp))

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> concat $
                        map (\(v,out) -> parse (f v) out) (parse p inp))

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           rs -> rs)

-- Derived primitives

-- verify that the parsed object satisfy a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) =  do char x
                    string xs
                    return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: (Num int, Read int) => Parser int
nat = do xs <- some digit
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- Parallel parsing: getting the results of both parsers
--  Use with caution: it can cause inefficiency

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> (parse p1 inp) ++ (parse p2 inp))


-- Simple example of parsing a function

simpleFun :: Parser (Int -> Int)
simpleFun = (symbol "double" >> return (\x -> 2*x))
            <|> (symbol "square" >> return (\x -> x^2))

binOp :: Parser (Int -> Int -> Int)
binOp = (symbol "+" >> return (+))
        <|> (symbol "*" >> return (*))
        <|> (symbol "-" >> return (-))

simpleOp :: Parser Int
simpleOp = do x  <- natural
              op <- binOp
              y  <- natural
              return (op x y)




-- Parsing a binary number
bit :: Parser Char
bit = sat (`elem` "01")

binary :: Parser Int
binary = token $
         do xs <- some bit
            return (binToDec (reverse xs))

binToDec :: String -> Int
binToDec "" = 0
binToDec ('0':bits) = 2 * binToDec bits
binToDec ('1':bits) = 1 + 2 * binToDec bits

intORbin :: Parser Int
intORbin = integer <||> binary
