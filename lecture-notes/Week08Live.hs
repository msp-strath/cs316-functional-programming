module Week08Live where

import Control.Monad
import Control.Monad.State

-------------------------------------------------------
-- Feeding various predicates to filterM

-- is the value bigger than its position in the list?
p :: Int -> State Int Bool
p n = do
  k <- get
  () <- put (k + 1)
  return (n > k)

-- is the value bigger than the ones we have seen so far?
q :: Int -> State Int Bool
q n = do
  k <- get
  if n < k
    then return False
    else do
      put n
      return True

-- Asking the user whether they like the value
r :: Int -> IO Bool
r n = do
  putStrLn ("Do you like " ++ show n ++ "?")
  l <- getLine
  case l of
    "wnk" -> return False
    "gg"  -> return True
    _ -> r n

-------------------------------------------------------
-- Revisint Print

newtype Print a = MkPrint { runPrint :: (a, [String]) }

instance Functor Print where
  fmap f (MkPrint (a, msg)) = MkPrint (f a, msg)

instance Monad Print where
  return x = MkPrint (x, [])
  MkPrint (x, msg0) >>= k =
    let (MkPrint (y, msg1)) = k x in
    MkPrint (y, msg0 ++ msg1)

instance Applicative Print where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return (f x)

-------------------------------------------------------
-- Generalising Print so that we don't commit to [String]
-- early on

newtype Writer w a = MkWriter { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (MkWriter (a, msg)) = MkWriter (f a, msg)

instance Monoid w => Monad (Writer w) where
  return x = MkWriter (x, mempty)
  MkWriter (x, msg0) >>= k =
    let (MkWriter (y, msg1)) = k x in
    MkWriter (y, msg0 <> msg1)

instance Monoid w => Applicative (Writer w) where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return (f x)



-------------------------------------------------------
-- TODO: pushing a configuration down

newtype Reader r a = MkReader { runReader :: r -> a }





-------------------------------------------------------
-- Parser combinators

-- Parse, don't validate
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/


-- If we can get input, how do we take it apart?
type Parser_V1 a = String -> Maybe a


-- DEFINE
-- boolean_V1 :: Parser_V1 Bool


-- DEFINE
-- orElse :: Parser_V1 a -> Parser_V1 a -> Parser_V1 a
-- disj :: Parser_V1 a -> Parser_V1 b -> Parser_V1 (Either a b)


-- DISCUSS limitations



-- DEFINE Parser

-- DEFINE
-- parse :: Parser a -> String -> Maybe a


-- DEFINE
-- orElse :: Parser a -> Parser a -> Parser a


-- DEFINE
-- disj :: Parser l -> Parser r -> Parser (Either l r)

-- DEFINE
-- boolean :: Parser Bool
-- (via failure, char, expect, string, etc.)


-- DEFINE many (0+), some (1+)

-- DEFINE pbetween, pwhitespace, ptoken, pparens, ppair



-- DEFINE pSepBy, plist, ptuple
