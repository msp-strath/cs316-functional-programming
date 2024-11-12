{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Week08Live where

import Control.Monad (ap)

import Prelude hiding (putChar, getChar)
import Data.Char          (toUpper, isDigit, digitToInt, isSpace)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar, stdin, stdout,
                           hClose, IOMode (..), hIsEOF, Handle)

{-    WEEK 8 : REAL I/O and PARSER COMBINATORS -}

{-    Part 8.1 : I/O Conceptually

   A great philosopher once wrote:

     The philosophers have only interpreted the world, in various
     ways. The point, however, is to change it.

      -- Karl Marx ( https://en.wikipedia.org/wiki/Theses_on_Feuerbach )

-}

data IOAction a
  = End a
  | Input  ()   (Char -> IOAction a)
  | Output Char (() -> IOAction a)

-- putChar :: Char -> ()

f x = (putChar x, putChar x)

f2 x = (z, z)
  where z = putChar x


f' x = do
  putChar x
  putChar x
--  return (l, r)

-- putChar x >>= (\l -> putChar x >>= (\r -> return (l,r)))

f2' x = do
  z <- putChar x
  return (z, z)


putChar :: Char -> IO ()
putChar = hPutChar stdout

getChar :: IO Char
getChar = hGetChar stdin

-- getChar

-- printLine

-- readLine
readLine :: IO String
readLine = go []
  where go xs = do
          c <- getChar
          if c == '\n' then
            return (reverse xs)
          else
            go (c:xs)



{- Part 8.4 : PARSER COMBINATORS -}

-- If we can get input, how do we take it apart?

type Parser_v1 a = String -> Maybe a

-- Parsing Booleans

boolean :: Parser_v1 Bool
boolean "True" = Just True
boolean "False" = Just False
boolean _ = Nothing

-- How to parse pairs of Booleans?

newtype Parser_v2 a = MkParser_v2 { runParser_v2 :: String -> Maybe (a, String) }
  deriving (Functor)

instance Applicative Parser_v2 where
  pure = return
  (<*>) = ap

instance Monad Parser_v2 where
  return x = MkParser_v2 (\ str -> Just (x, str))
  mx >>= mf = MkParser_v2 (\ str0 ->
                case runParser_v2 mx str0 of
                  Nothing -> Nothing
                  Just (x, str1) -> runParser_v2 (mf x) str1)

boolean_v2 :: Parser_v2 Bool
boolean_v2 = MkParser_v2 go where

  go ('T' : xs) = Just (True, xs)
  go ('F' : xs) = Just (False, xs)
  go _ = Nothing

string_v2 :: String -> Parser_v2 ()
string_v2 str = for_ str char_v2

char_v2 :: Char -> Parser_v2 ()
char_v2 c = MkParser_v2 go where

  go (x : xs)
    | c == x = Just ((), xs)
    | otherwise = Nothing
  go [] = Nothing

boolean2_v2 :: Parser_v2 (Bool, Bool)
boolean2_v2 = do
  char_v2 '('
  l <- boolean_v2
  char_v2 ','
  r <- boolean_v2
  char_v2 ')'
  return (l, r)

-- Sequencing! Monads!
