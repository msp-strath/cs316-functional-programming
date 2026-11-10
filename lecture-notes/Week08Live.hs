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
