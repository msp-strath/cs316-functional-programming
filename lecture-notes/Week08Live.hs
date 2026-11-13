{-# LANGUAGE DeriveFunctor #-}

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
boolean_V1 :: Parser_V1 Bool
boolean_V1 "True" = Just True
boolean_V1 "False" = Just False
boolean_V1  _      = Nothing

unit_V1 :: Parser_V1 ()
unit_V1 "()" = Just ()
unit_V1 _ = Nothing

orElseMaybe :: Maybe a -> Maybe a -> Maybe a
orElseMaybe (Just x) _ = Just x
orElseMaybe Nothing x  = x

-- DEFINE
orElse_V1 :: Parser_V1 a -> Parser_V1 a -> Parser_V1 a
orElse_V1 p1 p2 = (\str -> p1 str `orElseMaybe` p2 str)

disj_V1 :: Parser_V1 a -> Parser_V1 b -> Parser_V1 (Either a b)
disj_V1 p1 p2 = (fmap Left . p1) `orElse_V1` (fmap Right . p2)

-- DISCUSS limitations

newtype Parser a
  = MkParser
  { runParser :: String -> Maybe (a, String) }
  deriving (Functor)

-- DEFINE Parser

-- DEFINE
parse :: Parser a -> Parser_V1 a
parse (MkParser p) str = do
  (v, str) <- p str
  if null str then Just v else Nothing

punit :: Parser ()
punit = MkParser $ \ str -> case str of
  '(':')':str -> Just ((), str)
  _ -> Nothing

-- DEFINE
orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p) (MkParser q) = MkParser $ \ str ->
  (p str) `orElseMaybe` (q str)

-- DEFINE
disj :: Parser l -> Parser r -> Parser (Either l r)
disj p q = fmap Left p `orElse` fmap Right q

-- DEFINE
-- boolean :: Parser Bool
-- (via failure, char, expect, string, etc.)

failure :: Parser a
failure = MkParser $ \ str -> Nothing

pChar :: Parser Char
pChar = MkParser $ \ str -> case str of
  c:str -> Just (c, str)
  _ -> Nothing

instance Monad Parser where
  return x = MkParser (\ str -> Just (x, str))
  MkParser p >>= k = MkParser $ \ str0 -> do
    (v, str1) <- p str0
    runParser (k v) str1

instance Applicative Parser where
  pure = return
  (<*>) = ap

pexpect :: Char -> Parser ()
pexpect c = do
  c' <- pChar
  if c == c' then return () else failure

pstring :: String -> Parser ()
pstring str = forM_ str pexpect

pboolean :: Parser Bool
pboolean =
  fmap (\ () -> True) (pstring "True")
  `orElse`
  fmap (\ () -> False) (pstring "False")

-- DEFINE many (0+), some (1+)

many :: Parser a -> Parser [a]
many p = some p `orElse` return []

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  return (x : xs)

-- DEFINE pbetween, pwhitespace, ptoken, pparens, ppair

pbetween
  :: (Parser (), Parser ())
  -> Parser a -> Parser a
pbetween (l, r) m = do
  l
  x <- m
  r
  return x


-- DEFINE pSepBy, plist, ptuple
