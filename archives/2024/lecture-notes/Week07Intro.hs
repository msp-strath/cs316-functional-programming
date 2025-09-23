{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Intro where

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
  (>>=)  :: m a -> (a -> m b) -> m b


join :: m (m a) -> m a
join = undefined


-- Maybe monad
instance Monad Maybe where
  return a = Just a
  Nothing >>= k = Nothing
  Just a  >>= k = k a

failure :: Maybe a
failure = Nothing

{- 7.2 'do' NOTATION -}
{-
lookupList_v2 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList_v2 [] kvs = returnOk []
lookupList_v2 (k:ks) kvs =
  search k kvs         >>= \v ->
  lookupList_v2 ks kvs >>= \vs ->
  returnOk (v:vs)
-}
search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure
search k ((k',v):kvs) = if k == k' then return v else search k kvs

lookupList_v2 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList_v2 [] kvs = return []
lookupList_v2 (k:ks) kvs = do
  v  <- search k kvs;
  vs <- lookupList_v2 ks kvs;
  return (v:vs)


{- 7.3 STATE MONAD -}

newtype State a = MkState (Int -> (Int, a))

instance Monad State where
  return a = MkState (\s -> (s, a))

  -- t :: Int -> (Int, a)
  -- k :: a -> State b
  MkState t >>= k =
    MkState (\s0 -> let (s1, a)    = t s0
                        MkState t' = k a
                        (s2, b)    = t' s1
                    in (s2, b))

get :: State Int
get = MkState (\s -> (s,s))

put :: Int -> State ()
put s = MkState (\_ -> (s,()))


increment :: State ()
increment = do i <- get
               put (i+1)

modify :: (Int -> Int) -> State ()
modify f = do i <- get
              put (f i)

decrement = modify (\x -> x - 1)

numberList :: [a] -> State [(Int,a)]
numberList []     = return []
numberList (x:xs) =
  do i <- get;
     increment;
     ys <- numberList xs;
     return ((i,x):ys)

runState :: State a -> Int -> a
runState (MkState t) i = let (_,a) = t i in a

-- Most Monads come with:
--  (a) a collection of basic operations: failure, or get/put
--  (b) a 'run' function that executes the computation


{- 7.5 THINGS FOR ALL MONADS -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do y  <- f x
                   ys <- mapM f xs
                   return (y:ys)

numberList_v2 :: [a] -> State [(Int,a)]
numberList_v2 = mapM (\a -> do i <- get; increment; return (i,a))

-- mapM_
mapM_ :: Monad m => (a -> m ()) -> [a] -> m ()
mapM_ f [] = return ()
mapM_ f (x:xs) = do f x
                    mapM_ f xs

runState_ :: State a -> Int -> Int
runState_ (MkState t) i = let (i1,_) = t i in i1

addUpList :: [Int] -> State ()
addUpList = mapM_ (\i -> modify (+i))

for_ :: Monad m => [a] -> (a -> m ()) -> m ()
for_ = flip mapM_

addUpList :: [Int] -> State ()
addUpList xs = for_ xs $ \i ->
                 modify (+i)
