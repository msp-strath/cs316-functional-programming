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

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y


{-    WEEK 7 : MONADS

   Last week we saw three examples of how to simulate side effects
   with "pure" code in Haskell:

     1. simulating exceptions using the 'Maybe' type,

     2. simulating mutable state by explicit state passing, and

     3. simulating printing by collecting outputs.

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

-- Monad
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b   --- pronounced 'bind'

-- Maybe monad


instance Monad Maybe where
  return = Just
  Nothing >>= k = Nothing
  Just v  >>= k = k v

  -- return v >>= k     === k v
  -- c >>= return       === c
  -- (c >>= k1) >>= k2  === c >>= (\ x -> k1 x >>= k2)

apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply mf mx =
  mf >>= \ f ->
  mx >>= \ x ->
  return (f x)

filterM :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterM p [] = return []
filterM p (x : xs) =
  p x          >>= \ b ->
  filterM p xs >>= \ xs' ->
  return (if b then x : xs' else xs')

-- do Notation

apply_v2 :: Maybe (a -> b) -> Maybe a -> Maybe b
apply_v2 mf mx =
  do f <- mf
     x <- mx
     return (f x)

filterM_v2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterM_v2 p [] = do return []
filterM_v2 p (x : xs) = do
  b   <- p x
  xs' <- filterM_v2 p xs
  return (if b then x : xs' else xs')


-- State Monad

newtype State s a = MkState { runState :: s -> (a, s) }

instance Monad (State s) where
  return v = MkState (\ s -> (v, s))
  c1 >>= k = MkState (\ s0 ->
      let (a, s1) = runState c1 s0 in
      let (b, s2) = runState (k a) s1 in
      (b, s2))


apply_v3 :: State s (a -> b) -> State s a -> State s b
apply_v3 mf mx =
  do f <- mf
     x <- mx
     return (f x)

filterM_v3 :: (a -> State s Bool) -> [a] -> State s [a]
filterM_v3 p [] = do return []
filterM_v3 p (x : xs) = do
  b   <- p x
  xs' <- filterM_v3 p xs
  return (if b then x : xs' else xs')

-- Functions for all monads
apply_v4 :: Monad m => m (a -> b) -> m a -> m b
apply_v4 mf mx =
  do f <- mf
     x <- mx
     return (f x)

filterM_v4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM_v4 p [] = do return []
filterM_v4 p (x : xs) = do
  b   <- p x
  xs' <- filterM_v4 p xs
  return (if b then x : xs' else xs')

treeSort :: Monad m
         => (a -> a -> m Bool)
         -> [a]
         -> m [a]
treeSort cmp [] = return []
treeSort cmp (x : xs) = do
  lower  <- filterM_v4 (\y -> cmp x y) xs
  higher <- filterM_v4 (\y -> do r <- cmp x y; return (not r)) xs
  --                   (\y -> apply_v4 (return not) (cmp x y))
  --                   (\y -> not <$> cmp x y)
  lowerSorted <- treeSort cmp lower
  higherSorted <- treeSort cmp higher
  return (lowerSorted ++ [x] ++ higherSorted)

newtype Count a = MkCount { runCount :: (Int, a) }
  deriving Show

instance Monad Count where
  return x = MkCount (0, x)
  c >>= k = MkCount (let (count1,a) = runCount c in
                     let (count2,b) = runCount (k a) in
                     (count1+count2, b))

step :: Count ()
step = MkCount (1, ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x : xs) = do
  y <- f x
  ys <- mapM f xs
  return (y : ys)

mapM_ :: Monad m => (a -> m ()) -> [a] -> m ()
mapM_ f [] = return ()
mapM_ f (x : xs) = do
  _ <- f x
  _ <- mapM_ f xs
  return ()

for_ :: Monad m => [a] -> (a -> m ()) -> m ()
for_ xs f = mapM_ f xs

-- for_ [0..10] (\x -> do print x)
