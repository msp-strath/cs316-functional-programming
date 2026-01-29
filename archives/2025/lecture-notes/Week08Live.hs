{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Week08Live where

import Control.Monad (ap, forM)

import Prelude hiding (putChar, getChar)
import Data.Char          (toUpper, isDigit, digitToInt, isSpace, isAlpha)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar, stdin, stdout,
                           hClose, IOMode (..), hIsEOF, Handle)

import Debug.Trace

{-    WEEK 8 : REAL I/O and PARSER COMBINATORS -}

{-    Part 8.1 : I/O Conceptually

-}

-- DEFINE IOAction as a Monad

-- Do some IO and (hopefully) eventually deliver an a-value
data IOAction a
  = End a
  | GetChar ()   (Char -> IOAction a)
  | PutChar Char (()   -> IOAction a)

instance Show a => Show (IOAction a) where
  show (End a) = show a
  show (GetChar () k) = "GetChar"
  show (PutChar c k) = "PutChar " ++ [c]

instance Functor IOAction where
  fmap f io = case io of
    End a -> End (f a)
    GetChar () k -> GetChar () (fmap f . k)
    PutChar c  k -> PutChar c  (fmap f . k)

instance Applicative IOAction where
  pure = End
  (<*>) = ap

instance Monad IOAction where
  (>>=) :: IOAction a -> (a -> IOAction b) -> IOAction b
  End a         >>= k2 = k2 a
  GetChar () k1 >>= k2 = GetChar () ((>>= k2) . k1)
  PutChar c  k1 >>= k2 = PutChar c  ((>>= k2) . k1)

getCharAction :: IOAction Char
getCharAction = GetChar () End

putCharAction :: Char -> IOAction ()
putCharAction c = PutChar c End

getPutCharAction :: IOAction ()
getPutCharAction =
  GetChar () (\ c -> PutChar c End)

echo :: IOAction ()
echo = do
  c <- getCharAction
  putCharAction c



-- putChar :: Char -> ()

test x = (z,z)
   where z = x + 100

--putChar :: Char -> Char
--putChar x = x

f x = (trace ("my character: " ++ [x]) (putChar x)
      , trace ("my character: " ++ [x]) (putChar x))

f2 x = (z,z)
  where z = trace ("my character: " ++ [x]) (putChar x)

-- DEFINE f, f2
-- DISCUSS referential transparency



-- DEFINE f', f2'

f' x = do putChar x
          putChar x


f2' x = do z <- putChar x
           return (z,z)
-- DEFINE putChar

putChar :: Char -> IO ()
putChar  = hPutChar stdout

-- DEFINE getChar

getChar :: IO Char
getChar = hGetChar stdin


-- DEFINE printLine

printLine :: String -> IO ()
printLine ""     = putChar '\n'
printLine (x:xs) = do putChar x
                      printLine xs

printLine1 :: String -> IO ()
printLine1 str = do
  _ <- traverse putChar str
  putChar '\n'

-- DEFINE readLine
readLine :: IO String
readLine = go []
    where go xs = do c <- getChar
                     if c == '\n' then return (reverse xs)
                                  else go (c:xs)




{- Part 8.4 : PARSER COMBINATORS -}

-- If we can get input, how do we take it apart?
type Parser_V1 a = String -> Maybe a

boolean_V1 :: Parser_V1 Bool
boolean_V1 "True" = Just True
boolean_V1 "False" = Just False
boolean_V1 _ = Nothing -- " True"


newtype Parser a = MkParser {runParser :: String -> Maybe (a,String)}
 deriving Functor

instance Applicative Parser where
  pure x = MkParser  (\str -> Just (x,str))
  (<*>)  = ap

instance Monad Parser where
  mx >>= mf = MkParser (\str0 ->
                           case runParser mx str0 of
                               Nothing -> Nothing
                               Just (x,str1) -> runParser (mf x) str1)
orElseMaybe :: Maybe a -> Maybe a -> Maybe a
orElseMaybe (Just x) _ = Just x
orElseMaybe Nothing x  = x

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = MkParser (\str -> runParser p1 str `orElseMaybe` runParser p2 str)

disj :: Parser l -> Parser r -> Parser (Either l r)
disj l r = fmap Left l `orElse` fmap Right r


-- type Parser_v1 a = String -> Maybe a

-- Parsing Booleans

-- boolean_v1 :: Parser_v1 Bool

failure :: Parser a
failure = MkParser (\_ -> Nothing)

char :: Parser Char
char = MkParser go
   where go (x:xs) = Just (x,xs)
         go []     = Nothing

expect :: (Char -> Bool) -> Parser Char
expect p = do
  c <- char
  if p c then return c else failure

-- How to parse pairs of Booleans?
expectChar :: Char -> Parser ()
expectChar c = () <$ expect (c ==)


-- for [1..10] $ \ i -> (...)

string :: String -> Parser ()
string str = for_ str (\c -> expectChar c)

boolean :: Parser Bool
boolean = do string "True"
             return True
          `orElse`
          do string "False"
             return False


many :: Parser a -> Parser [a]
many pa = some pa `orElse` return []

some :: Parser a -> Parser [a]
some pa = do
  a <- pa
  as <- many pa
  return (a : as)

whitespace :: Parser ()
whitespace = () <$ many (expect (`elem` " \t\r\n"))

token :: String -> Parser ()
token tk = do
  whitespace
  string tk
  whitespace

pbetween
  :: Parser ()
  -> Parser ()
  -> Parser a
  -> Parser a
pbetween pstart pend pval = do
  pstart
  val <- pval
  pend
  return val

pparens :: Parser a -> Parser a
pparens = pbetween (token "(") (token ")")

ppair :: Parser a -> Parser b -> Parser (a, b)
ppair pa pb = pparens $ do
  l <- pa
  token ","
  r <- pb
  return (l,r)

pSepBy :: Parser () -> Parser a -> Parser [a]
pSepBy psep pval =
  do val <- pval
     vals <- many (do psep; pval)
     return (val : vals)
  `orElse` return []

plist :: Parser a -> Parser [a]
plist
  = pbetween (token "[") (token "]")
  . pSepBy (token ",")

boolean2 :: Parser (Bool,Bool)
boolean2 = ppair boolean boolean

parse :: Parser a -> String -> Maybe a
parse p str = case runParser p str of
  Nothing -> Nothing
  Just (x, []) -> Just x
  Just (x, _) -> Nothing




















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


{-
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
-}
