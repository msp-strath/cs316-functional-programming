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
type Coin = Int
type Till = [Coin]
type Amount = Int
type Change = [Coin]

-- DISCUSS how Till, Change, Coin, Amount relate
-- (e.g. define a function turning X into Y)

tillTotal :: Till -> Amount
tillTotal = sum
{-
tillTotal [] = 0
tillTotal (c : cs) = c + tillTotal cs
-}

changeTotal :: Change -> Amount
changeTotal = sum

-- PONDER makeChange, a function that takes:
-- a till
-- an amount
-- and returns change matching the amount

-- WRITE some tests

-- 1. Unit tests
-- Till with exactly the right coin
-- Till with [1..10] and amount of 55

noCoin :: Bool
noCoin = makeChange [1..10] 0 [] == Just []

rightCoin :: Bool
rightCoin = (makeChange [5] 5 []) == Just [5]

wholeTill :: Bool
wholeTill = (makeChange [1..15] 55 []) == Just [1..10]

-- 2. Property testing
-- What property do we expect the outcome to verify?

prop_makeChange1 :: Till -> Amount -> Property
prop_makeChange1 till amount
  = let res = makeChange (map abs till) (abs amount) [] in
    isJust res ==> changeTotal (fromMaybe [] res) == abs amount

{-
prop_makeChange2 :: Till -> Amount -> Bool
prop_makeChange2 till amount =
  tillTotal till >= changeTotal (makeChange till amount [])
-}
-- DEFINE makeChange

makeChange :: Till -> Amount -> Change -> Maybe Change
makeChange _ 0 acc = Just (reverse acc)
makeChange (coin:till) n acc
  | n >= coin = makeChange till (n-coin) (coin:acc)
  | otherwise = makeChange till n acc
makeChange [] n acc = Nothing


-- TEST makeChange
-- quickCheck, verboseCheck


-- FIX (?) makeChange


-- TEST new version
-- TEST with precondition (==>)
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
