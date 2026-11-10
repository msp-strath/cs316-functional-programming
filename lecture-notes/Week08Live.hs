module Week08Live where

import Control.Monad
import Control.Monad.State

p :: Int -> State Int Bool
p n = do
  k <- get
  () <- put (k + 1)
  return (n > k)

q :: Int -> State Int Bool
q n = do
  k <- get
  if n < k
    then return False
    else do
      put n
      return True

r :: Int -> IO Bool
r n = do
  putStrLn ("Do you like " ++ show n ++ "?")
  l <- getLine
  case l of
    "wnk" -> return False
    "gg"  -> return True
    _ -> r n



newtype Print a = MkPrint { runPrint :: (a, [String]) }

instance Functor Print where
  fmap f = _a

instance Monad Print where
  return = _b
  (>>=) = _c

instance Applicative Print where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return (f x)
