module Week08Live where

import Control.Monad
import Control.Monad.State

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
