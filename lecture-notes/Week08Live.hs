{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Week08Live where

import Control.Monad (ap)

import Prelude hiding (putChar, getChar)
import Data.Char          (toUpper, isDigit, digitToInt, isSpace, isAlpha)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar, stdin, stdout,
                           hClose, IOMode (..), hIsEOF, Handle)

{-    WEEK 8 : REAL I/O and PARSER COMBINATORS -}

{-    Part 8.1 : I/O Conceptually

-}

-- DEFINE IOAction as a Monad

-- putChar :: Char -> ()

-- DEFINE f, f2
-- DISCUSS referential transparency


-- DEFINE f', f2'


-- DEFINE putChar

-- DEFINE getChar

-- DEFINE printLine

-- DEFINE readLine




{- Part 8.4 : PARSER COMBINATORS -}

-- If we can get input, how do we take it apart?

-- type Parser_v1 a = String -> Maybe a

-- Parsing Booleans

-- boolean_v1 :: Parser_v1 Bool

-- How to parse pairs of Booleans?

-- Problem: these parsers are "monolithic". There is no way to access
-- the trailing input they couldn't parse.


-- Solution: Parsing with Leftovers
-- newtype Parser a = MkParser { runParser :: String -> Maybe (a, String) }
--  deriving (Functor)

-- runParser :: Parser a -> String -> Maybe (a, String)
-- runParser (MkParser p) = p

-- See next week
-- instance Applicative Parser where

-- instance Monad Parser where


-- char :: Parser Char

-- orElseMaybe :: Maybe a -> Maybe a -> Maybe a

-- orElse :: Parser a -> Parser a -> Parser a

-- failure :: Parser a




{-  The basic parser interface:


    Parser a
       ^--- represents a parser of things of type 'a'

    return :: a -> Parser a
       ^--- parse nothing and return 'a'

    (>>=)  :: Parser a -> (a -> Parser b) -> Parser b
       ^--- sequence two parsers, feeding the output of the first into the second

    orElse :: Parser a -> Parser a -> Parser a
       ^--- try one parser, if that fails try the other parser

    failure :: Parser a
       ^--- always fail

    char :: Parser Char
       ^--- read one character from the input
-}



-- Examples:

-- expectChar :: Char -> Parser ()

-- string :: String -> Parser ()

-- boolean :: Parser Bool

-- boolean2 :: Parser (Bool, Bool)

-- eof :: Parser ()







{- PLAN: write a parser for an expression language using the combinators. -}

-- 1. Fix a grammar

{-   <expr> ::= <mulexpr> + <expr>
              | <mulexpr>

     <mulexpr> ::= <baseexpr> * <mulexpr>
                 | <baseexpr>

     <baseexpr> ::= <number>
                  | <variable>
                  | <variable> ( <expr>* )     { separated by commas }
                  | ( <expr> )

     <number> ::= [0-9]+
      (one or more of characters in 0 .. 9)

     <variable> ::= [A-Za-z]+
      (one or more of alphabetic characters)
-}

data Expr
  = Addition MultExpr Expr
  | AMultExpr MultExpr
  deriving Show

data MultExpr
  = Multiplication BaseExpr MultExpr
  | ABaseExpr BaseExpr
  deriving Show

data BaseExpr
  = Number Integer
  | Variable String
  | FunCall String [Expr]
  | Parens Expr
  deriving Show

-- 2. Design an Abstract Syntax Tree type

-- 2.1: the datatype

-- 2.2: a simple evaluator

whitespace = satisfies isSpace

whitespaces = zeroOrMore whitespace

expr :: Parser Expr
expr =
  do me <- multExpr
     whitespaces
     expectChar '+'
     whitespaces
     fe <- expr
     return (Addition me fe)
  `orElse`
  do me <- multExpr
     return (AMultExpr me)

multExpr :: Parser MultExpr
multExpr =
  do be <- baseExpr
     whitespaces
     expectChar '*'
     whitespaces
     fe <- multExpr
     return (Multiplication be fe)
  `orElse`
  do be <- baseExpr
     return (ABaseExpr be)

oneOrMore  :: Parser a -> Parser [a]
zeroOrMore :: Parser a -> Parser [a]

oneOrMore p = do
  x <- p
  xs <- zeroOrMore p
  return (x : xs)

zeroOrMore p = oneOrMore p `orElse` return []

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p =
  do x <- p
     xs <- zeroOrMore (do sep; p)
     return (x:xs)
  `orElse`
  return []

baseExpr :: Parser BaseExpr
baseExpr =
  do n <- number
     return (Number n)
  `orElse`
  do fnm <- variable
     whitespaces
     expectChar '('
     arguments <- sepBy (expectChar ',') expr
     expectChar ')'
     return (FunCall fnm arguments)
  `orElse`
  do v <- variable
     return (Variable v)
  `orElse`
  do expectChar '('
     whitespaces
     e <- expr
     whitespaces
     expectChar ')'
     return (Parens e)

fullExpr :: Parser Expr
fullExpr = do whitespaces; e <- expr; whitespaces; eof; return e

number :: Parser Integer
number = do
  ds <- oneOrMore digit
  return (read ds)

satisfies :: (Char -> Bool) -> Parser Char
satisfies pred = do
  c <- char
  if pred c then return c else failure

digit = satisfies isDigit
alpha = satisfies isAlpha

variable :: Parser String
variable = oneOrMore alpha

-- 3. Write a parser, following the grammar

-- 3.1: oneOrMore, alphabetic, number



-- 3.2: expr, mulExpr, baseExpr

-- 3.4: whitespace
