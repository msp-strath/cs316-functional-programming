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


-- TEST makeChange
-- quickCheck, verboseCheck


-- FIX (?) makeChange


-- TEST new version
-- TEST with precondition (==>)
-- TEST with better inputs


-- REFACTOR (?) makeChange
