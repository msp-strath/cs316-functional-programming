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

------------------------------------------------------------------------
-- Monad

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b   --- pronounced 'bind'

-- DEFINE Maybe monad


-- DISCUSS laws



-- DEFINE apply :: Maybe (a -> b) -> Maybe a -> Maybe b
-- USING >>= explicitly


-- DEFINE filterM :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- USING >>= explicitly



------------------------------------------------------------------------
-- do Notation

-- REFACTOR apply_v2
-- USING do notations

-- REFACTOR filterM_v2
-- USING do notations



------------------------------------------------------------------------
-- State Monad

-- DEFINE State Monad

-- DEFINE apply_v3 for the State Monad
-- USING do notations

-- DEFINE filterM_v3 for the State Monad
-- USING do notations


-- DISCUSS similarities

------------------------------------------------------------------------
-- One function to rule them all


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





-- DEFINE the Count Monad (for complexity analysis)


-- DEFINE mapM, mapM_, forM_, etc.
