{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Live where

import Data.Maybe
import Test.QuickCheck

------------------------------------------------------------------------
-- Motivating example

-- Making change: you have a till and have to give some money back to
-- a customer. First: let's model the domain of discourse!
--
-- DEFINE Coin
-- DEFINE Till
-- DEFINE Amount
-- DEFINE Change



-- DISCUSS how Till, Change, Coin, Amount relate
-- (e.g. define a function turning X into Y)



-- PONDER makeChange, a function that takes:
-- a till
-- an amount
-- and returns change matching the amount

-- WRITE some tests

-- 1. Unit tests
-- Till with exactly the right coin
-- Till with [1..10] and amount of 55

-- 2. Property testing
-- What property do we expect the outcome to verify?




-- DEFINE makeChange



makeChange :: [Int] -> Int -> Maybe [Int]
makeChange _ 0 = Just []
makeChange (c : cs) n
  | c <= n    = (c :) <$> makeChange cs (n - c)
  | otherwise = makeChange cs n
makeChange _ _ = Nothing

-- TEST makeChange
-- quickCheck, verboseCheck


-- FIX (?) makeChange


-- TEST new version
-- TEST with precondition
-- TEST with better inputs


-- REFACTOR (?) makeChange



------------------------------------------------------------------------------
-- The Domain Specific Language of Failure

-- Instead of doing case analysis, using Maybe, Nothing, Just, let's try
-- to abstract a little bit away from that.

-- DEFINE OOPS, the type of potentially failing computations
-- DEFINE successOOPS
-- DEFINE failureOOPS
-- DEFINE orElseOOPS


-- REFACTOR makeChange as makeChangeOOPS



-- DISCUSS the impact of greed on computing change

-- DEFINE BRRR, the type of computations exploring ALL possibilities
-- DEFINE successBRRR
-- DEFINE failureBRR
-- DEFINE orElseBRR

-- Search & replace over makeChangeOOPS to build makeChangeBRRR


------------------------------------------------------------------------------
-- Search tree vs. Search strategy

-- In Haskell we can reify control flow; use:
-- DEFINE Success instead of success(OOPS/BRRR)
-- DEFINE Failure instead of failure(OOPS/BRRR)
-- DEFINE OrElse  instead of orElse(OOPS/BRRR)



-- BUILD makeChange


-- DEFINE greedy
-- DEFINE firstChoice
-- DEFINE allChoices
-- DEFINE best (using an (a -> Int) measure)
