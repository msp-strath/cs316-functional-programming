{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week08Live where

import Data.Char          (toUpper, isDigit, digitToInt, isSpace)
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

isChar :: Char -> Parser ()
isChar c = bind char (\c' -> if c == c' then nothing () else failure)

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

isString :: String -> Parser ()
isString str = for_ str (\c -> isChar c)

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

-- 1+2+3+(4+5)

digit :: Parser Int
digit = do
  c <- char
  if isDigit c then
    return (digitToInt c)
  else
    failure

expr :: Parser Int
expr =
  do e1 <- baseExpr
     isChar '+'
     e2 <- expr
     return (e1 + e2)
  `orElse`
  do e1 <- baseExpr
     isChar '*'
     e2 <- expr
     return (e1 * e2)
  `orElse`
  baseExpr

baseExpr :: Parser Int
baseExpr =
  do d <- digit
     return d
  `orElse`
  do isChar '('
     e <- expr
     isChar ')'
     return e
