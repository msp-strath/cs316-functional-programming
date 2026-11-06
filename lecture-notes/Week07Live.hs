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
import Data.Type

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

       Just      :: a -> Maybe a
       succeed   :: a -> Printing a
       ioSuccess :: a -> InputOutput a

   and a "do this, then do that" operation:

     andThen :: Maybe a
             -> (a -> Maybe b)
             -> Maybe b
     andTHEN :: Printing (Maybe a)
             -> (a -> Printing (Maybe b))
             -> Printing (Maybe b)
     ioThen  :: InputOutput a
             -> (a -> InputOutput b)
             -> InputOutput b

   The Week 06 tutorial questions asked you to write this function for

       ifOK :: Result a
            -> (a -> Result b)
            -> Result b
       sequ :: Process a
            -> (a -> Process b)
            -> Process b
-}

------------------------------------------------------------------------
-- Monad

class Monad (planFor :: Type -> Type) where
  return :: a -> planFor a
  (>>=)  :: planFor a
         -> (a -> planFor b)
         -> planFor b   --- pronounced 'bind'

-- DEFINE Maybe monad
instance Monad Maybe where
  return = Just
  Nothing >>= k = Nothing
  Just a  >>= k = k a

data Potato a = Potato deriving Show
instance Monad Potato where
  return _ = Potato
  Potato >>= _ = Potato

newtype Identity a = MkIdentity { runIdentity :: a }
instance Monad Identity where
  return a = MkIdentity a
  MkIdentity a >>= k = k a

-- DISCUSS laws
-- return v >>= k = k v
-- c >>= return   = c
-- (c >>= k1) >>= k2 = c >>= (\x -> k1 x >>= k2)


-- DEFINE apply :: Maybe (a -> b) -> Maybe a -> Maybe b
-- USING >>= explicitly, do blocks
apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply mf mx =
  mf >>= \ f ->
  mx >>= \ x ->
  return (f x)

(<*>) :: Monad m => m (a -> b) -> m a -> m b
pf <*> px =
  pf >>= \ f ->
  px >>= \ x ->
  return (f x)
infixl 9 <*>

apply2 :: Maybe (a -> b) -> Maybe a -> Maybe b
apply2 mf mx = do
  f <- mf
    {- this is still in the (f <- ...) block -}
  x <- mx
  return (f x)

-- DEFINE filterM :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- USING >>= explicitly, do blocks

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x : xs) = do
  b <- p x
  ys <- filterM p xs
  return (if b then x:ys else ys)

------------------------------------------------------------------------
-- State Monad

-- DEFINE State Monad

newtype State s a
  = MkState { runState :: s -> (s, a) }

instance Monad (State s) where
  return a = MkState (\ s0 -> (s0, a))
  (MkState fa) >>= k = MkState $ \ s0 ->
    let (s1, a) = fa s0 in
    let (MkState fb) = k a in
    let (s2, b) = fb s1 in
    (s2, b)

get :: State s s
get = MkState (\ s0 -> (s0, s0))

put :: s -> State s ()
put s1 = MkState (\ s0 -> (s1, ()))

p :: Int -> State Int Bool
p n = do
  k <- get
  put (k + 1)
  return (n > k)
