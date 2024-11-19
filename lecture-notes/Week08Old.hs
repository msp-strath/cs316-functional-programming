{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week08Live where

import Data.Char          (toUpper, isDigit, digitToInt, isSpace, isAlpha)
import Data.Foldable      (for_)


--- REMINDER:     Week 09 (Resit) class test
--                -- Wednesday 15th Nov -> Thursday 16th Nov (12:00noon)


-- Parsing

-- "asd,klj","sdjhds",90,fdfhg,kjh

-- { "a": [1,2], "b": {"c": [ 1,2,3 sdf], "d": null }

type Parser_v1 a = String -> Maybe a

boolParser_v1 :: Parser_v1 Bool
boolParser_v1 "true"  = Just True
boolParser_v1 "false" = Just False
boolParser_v1 _       = Nothing

-- truefalse  => (True, False)

-- input -> splitOn ' ' -> (boolParser, boolParser)

newtype Parser a = MkParser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) s = p s

boolParser :: Parser Bool
boolParser = MkParser (\input ->
                         case input of
                           't':'r':'u':'e':rest -> Just (True, rest)
                           'f':'a':'l':'s':'e':rest -> Just (False, rest)
                           _ -> Nothing)

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MkParser p1) (MkParser p2) =
  MkParser (\input ->
              case p1 input of
                Just (a, rest) ->
                  case p2 rest of
                    Just (b, final) -> Just ((a,b), final)
                    Nothing -> Nothing
                Nothing -> Nothing)

char :: Parser Char
char = MkParser (\input -> case input of
                             c:cs -> Just (c, cs)
                             []   -> Nothing)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (MkParser p) k =
  MkParser (\input ->
              case p input of
                Just (a, rest) ->
                  case k a of
                    MkParser p2 ->
                      case p2 rest of
                        Just (b, final) -> Just (b, final)
                        Nothing -> Nothing
                Nothing -> Nothing)

nothing :: a -> Parser a
nothing a = MkParser (\input -> Just (a, input))

andThen2 :: Parser a -> Parser b -> Parser (a,b)
andThen2 p1 p2 =
  bind p1 (\a -> bind p2 (\b -> nothing (a,b)))

-- isChar :: Char -> Parser ()
-- isChar c = bind char (\c' -> if c == c' then nothing () else failure)

failure :: Parser a
failure = MkParser (\input -> Nothing)

instance Monad Parser where
  (>>=) = bind

instance Applicative Parser where
  pure = nothing
  f <*> a = do x <- f; y <- a; return (x y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (MkParser p) =
    MkParser (\input -> case p input of
                          Just (a, rest) -> Just (f a, rest)
                          Nothing        -> Nothing)

pairOfBools :: Parser (Bool, Bool)
pairOfBools =
  do b1 <- boolParser
     isChar ','
     b2 <- boolParser
     return (b1, b2)

trueP :: Parser Bool
trueP =
  do isString "true"
     return True

falseP :: Parser Bool
falseP =
  do isString "false"
     return False

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\input ->
              case p1 input of
                Nothing -> p2 input
                Just (a, rest) -> Just (a,rest))



boolParser_v2 :: Parser Bool
boolParser_v2 = trueP `orElse` falseP

{-  <exp> := ...
           | true
           | false
-}

{-  Parser a     <--- represents a parser of things of type 'a'

    return :: a -> Parser a    <-- parse nothing and return 'a'

    (>>=)  :: Parser a -> (a -> Parser b) -> Parser b

    orElse :: Parser a -> Parser a -> Parser a

    failure :: Parser a

    char :: Parser Char
-}

isChar :: Char -> Parser ()
isChar c =
  do c' <- char
     if c == c' then return () else failure

isString :: String -> Parser ()
isString str = for_ str (\c -> isChar c)


{- PLAN: write a parser for an expression language using the combinators. -}

-- 1. Fix a grammar

{-   <expr> ::= <mulexpr> + <expr>
              | <mulexpr>

     <mulexpr> ::= <baseexpr> * <mulexpr>
                 | <baseexpr>

     <baseexpr> ::= <number>
                  | <variable>
                  | ( <expr> )

     <number> ::= [0-9]+
      (one or more of characters in 0 .. 9)

     <variable> ::= [A-Za-z]+
      (one or more of alphabetic characters)
-}

-- 2. Design an Abstract Syntax Tree type

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Variable String
  | FunCall String [Expr]
  | Number   Integer
  deriving (Show, Eq)

-- 3. Write a parser

whitespace :: Parser ()
whitespace =
  do zeroOrMore (isChar ' ')
     return ()

expr :: Parser Expr
expr =
  do e1 <- mulexpr
     whitespace
     isChar '+'
     whitespace
     e2 <- expr
     return (Add e1 e2)
  `orElse`
  do e <- mulexpr
     return e

mulexpr :: Parser Expr
mulexpr =
  do e1 <- baseexpr
     whitespace
     isChar '*'
     whitespace
     e2 <- mulexpr
     return (Mul e1 e2)
  `orElse`
  do e <- baseexpr
     return e

baseexpr :: Parser Expr
baseexpr =
  do n <- number
     return (Number n)
  `orElse`
  do f <- variable
     isChar '('
     args <- sepBy (isChar ',') expr
     isChar ')'
     return (FunCall f args)
  `orElse`
  do v <- variable
     return (Variable v)
  `orElse`
  do isChar '('
     whitespace
     e <- expr
     whitespace
     isChar ')'
     return e

wholeExpr :: Parser Expr
wholeExpr =
  do whitespace
     e <- expr
     whitespace
     return e

-- Plan for number:
-- 1. write a parser for digits 0-9
-- 2. write a parser for sequences of digits
-- 3. turn lists of digits into numbers

digit :: Parser Integer
digit =
  do c <- char
     case c of
       '0' -> return 0
       '1' -> return 1
       '2' -> return 2
       '3' -> return 3
       '4' -> return 4
       '5' -> return 5
       '6' -> return 6
       '7' -> return 7
       '8' -> return 8
       '9' -> return 9
       _   -> failure

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do d <- p
     ds <- zeroOrMore p
     return (d:ds)
  `orElse`
  return []

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p =
  do x <- p
     xs <- zeroOrMore (do sep; p)
     return (x:xs)
  `orElse`
  return []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do d <- p
                 ds <- zeroOrMore p
                 return (d:ds)

number :: Parser Integer
number =
  do ds <- oneOrMore digit
     return (fromDigits ds)

fromDigits :: [Integer] -> Integer
fromDigits = foldl (\n d -> n*10 + d) 0

alphabetic :: Parser Char
alphabetic =
  do c <- char
     if isAlpha c then return c else failure

variable :: Parser String
variable = oneOrMore alphabetic
