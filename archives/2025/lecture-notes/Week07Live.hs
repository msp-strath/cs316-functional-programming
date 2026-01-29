{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Live where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Data.Char      (isDigit, digitToInt)

import Data.Monoid

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y


{-    WEEK 7 : MONADS

   Last week we saw two examples of how to simulate side effects
   with "pure" code in Haskell:

     1. simulating exceptions using the 'Maybe' type,

     2. simulating printing by collecting outputs.

   This week, we look at the common pattern in all these examples, and
   give it a name: 'Monad'. -}


{- 7.1 DEFINING MONADS and THE MAYBE MONAD

       returnOk       :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a

   and a "do this, then do that" operation:

     ifOK                :: Maybe a ->    (a -> Maybe b)    -> Maybe b
     andThen             :: State a ->    (a -> State b)    -> State b
     andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b

   The Week 06 tutorial questions asked you to write this function for
   'Process'es, with yet again a similar type.

       sequ                :: Process a ->  (a -> Process b)  -> Process b
-}

------------------------------------------------------------------------
-- Monad

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b   --- pronounced 'bind'

-- DEFINE Maybe monad
instance Monad Maybe where
  return = Just
  Nothing >>= k = Nothing -- k stands for 'kontinuation'
  Just v >>= k  = (k v)

-- DISCUSS laws
-- return v >>= k = k v
-- c >>= return   = c
-- (c >>= k1) >>= k2 = c >>= (\x -> k1 x >>= k2)

apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply mf mx =
    mf >>= \f ->
    mx >>= \x ->
    return (f x)


apply2 :: Maybe (a -> b) -> Maybe a -> Maybe b
apply2 mf mx =
  do f <- mf
     x <- mx
     return (f x)

-- DEFINE apply :: Maybe (a -> b) -> Maybe a -> Maybe b
-- USING >>= explicitly



-- DEFINE filterM :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- USING >>= explicitly

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe p []     = return []
filterMaybe p (x:xs) =
    p x >>= \b ->
    filterMaybe p xs >>= \xs' ->
    return (if b then x:xs' else xs')

filterM2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterM2 p []     = return []
filterM2 p (x:xs) =
    do b   <- p x
       xs' <- filterM2 p xs
       return (if b then x:xs' else xs')

------------------------------------------------------------------------
-- do Notation




-- REFACTOR apply_v2
-- USING do notations

-- REFACTOR filterM_v2
-- USING do notations



------------------------------------------------------------------------
-- State Monad

-- DEFINE State Monad

newtype State s a
  = MkState { runState :: s -> (s, a) }

instance Monad (State s) where
  return v = MkState (\ s0 -> (s0, v))
  MkState act1 >>= k = MkState $ \ s0 ->
    let (s1, va) = act1 s0 in
    let MkState act2 = k va in
    let res@(s2, vb) = act2 s1 in
    res

-- DEFINE apply_v3 for the State Monad
-- USING do notations

apply3 :: State s (a -> b) -> State s a -> State s b
apply3 mf mx =
  do f <- mf
     x <- mx
     return (f x)

-- DEFINE filterM_v3 for the State Monad
-- USING do notations

filterM3 :: (a -> State s Bool) -> [a] -> State s [a]
filterM3 p []     = return []
filterM3 p (x:xs) =
    do b   <- p x
       xs' <- filterM3 p xs
       return (if b then x:xs' else xs')

pack :: Int -> [Int] -> [Int]
pack capacity items
  = snd $ runState (filterM3 decision items) capacity

  where
    decision :: Int -> State Int Bool
    decision item = MkState $ \ capacity ->
      if item <= capacity then (capacity - item, True)
      else (capacity, False)

{-
ghci> filterM2 (\ i -> if i == 0 then Nothing else Just True) [1..10]
Just [1,2,3,4,5,6,7,8,9,10]
ghci> filterM2 (\ i -> if i == 0 then Nothing else Just (i `mod` 2 == 0)) [1..10]
Just [2,4,6,8,10]
ghci> filterM2 (\ i -> if i == 0 then Nothing else Just (i `mod` 2 == 0)) [0..10]
Nothing

ghci> runState (filterM3 (\ i -> MkState (\ s -> (1+s, i<s))) [0..10]) 0
(11,[])
ghci> runState (filterM3 (\ i -> MkState (\ s -> (1+s, i<s))) [0..10]) 1
(12,[0,1,2,3,4,5,6,7,8,9,10])
ghci> runState (filterM3 (\ i -> MkState (\ s -> (1+s, i<s))) [0,2..10]) 1
(7,[0])
ghci> runState (filterM3 (\ i -> MkState (\ s -> (1+s, i<s))) [0,2..10]) 3
-}

-- DISCUSS similarities

------------------------------------------------------------------------
-- One function to rule them all

applyM :: Monad m => m (a -> b) -> m a -> m b
applyM mf mx =
  do f <- mf
     x <- mx
     return (f x)


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) =
    do b   <- p x
       xs' <- filterM p xs
       return (if b then x:xs' else xs')


-- DEFINE apply_v4 for all Monads
-- USING do notations

-- DEFINE filterM_v4 for all Monad
-- USING do notations





------------------------------------------------------------------------
-- More Monad-generic code

-- DEFINE
-- treeSort :: Monad m
--         => (a -> a -> m Bool)
--         -> [a]
--         -> m [a]
-- using filterM

treeSort :: Monad m
        => (a -> a -> m Bool)
        -> [a]
        -> m [a]
treeSort cmp [] = return []
treeSort cmp (v : vs) = do
  smoll <- filterM (\ w -> cmp w v) vs
  biggg <- filterM (\ w -> do { b <- cmp w v; return (not b) }) vs
  leftt <- treeSort cmp smoll
  right <- treeSort cmp biggg
  return (leftt ++ [v] ++ right)

newtype Identity a = MkIdentity { runIdentity :: a }

instance Monad Identity where
  return v = MkIdentity v
  MkIdentity v >>= f = f v

newtype Count m a = MkCount { runCount :: (m, a) }

instance Monoid m => Monad (Count m) where
  return v = MkCount (mempty, v)
  MkCount (m1, v1) >>= k =
    let MkCount (m2, v2) = k v1 in
    MkCount (m1 <> m2, v2)


tick :: a -> Count (Sum Int) a
tick v = MkCount (Sum 1, v)

getCount :: Count m a -> m
getCount (MkCount (m, _)) = m

-- DEFINE the Count Monad (for complexity analysis)

complexity
  :: (a -> a -> Bool)
  -> [a]
  -> Int
complexity cmp xs =
  let c = treeSort (\ v w -> tick (cmp v w)) xs in
  getSum $ getCount c

logg :: (Show a, Show b)
     => (a -> b -> c)
     -> (a -> b -> IO c)
logg cmp a b = do
  let str = unwords ["Checking", show a, "against", show b]
  putStrLn str
  return (cmp a b)

logging
  :: Show a
  => (a -> a -> Bool)
  -> [a]
  -> IO ()
logging cmp xs = do
  _ <- treeSort (logg cmp) xs
  return ()



-- DEFINE mapM, mapM_, forM_, etc.
