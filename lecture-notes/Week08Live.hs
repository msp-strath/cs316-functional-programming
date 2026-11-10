module Week08Live where

import Control.Monad
import Control.Monad.State

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x : xs) = do
  b <- p x
  ys <- filterM p xs
  return (if b then x:ys else ys)

p :: Int -> State Int Bool
p n = do
  k <- get
  () <- put (k + 1)
  return (n > k)

q :: Int -> IO Bool
q n = do
  putStrLn ("Do you like " ++ show n ++ "?")
  l <- getLine
  case l of
    "wnk" -> return False
    "gg"  -> return True
    _ -> q n
