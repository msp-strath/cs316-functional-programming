{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Live where

import Data.List
import Data.Maybe
import Test.QuickCheck

------------------------------------------------------------------------
-- Motivating example

-- Making change: you have a till and have to give some money back to
-- a customer. First: let's model the domain of discourse!

-- modelling

type Coin = Int
type Till = [Coin]
type Amount = Int
type Change = [Coin]

tillTotal :: Till -> Amount
tillTotal = sum

changeTotal :: Change -> Amount
changeTotal = sum

-- unit tests

noCoin :: Bool
noCoin = makeChange [1..10] 0 [] == Just []

rightCoin :: Bool
rightCoin = (makeChange [5] 5 []) == Just [5]

wholeTill :: Bool
wholeTill = (makeChange [1..15] 55 []) == Just [1..10]

-- property test
-- "if we get a result then it's the right amount"

prop_makeChange1 :: Till -> Amount -> Property
prop_makeChange1 = till amount ->
  let res = makeChange (map abs till) (abs amount) [] in
  isJust res ==> changeTotal (fromMaybe [] res) == abs amount

-- DEFINE makeChange

makeChange :: Till -> Amount -> Change -> Maybe Change
makeChange _ 0 acc = Just (reverse acc)
makeChange (coin:till) n acc
  | n >= coin = makeChange till (n-coin) (coin:acc)
  | otherwise = makeChange till n acc
makeChange [] n acc = Nothing


-- GA
-- property test
-- "if there is a valid subset of coins, we successfully get change"



-- AL
-- DISCUSS flaw
-- FIX makeChange




------------------------------------------------------------------------------
-- The Domain Specific Language of Failure

-- Instead of doing case analysis, using Maybe, Nothing, Just, let's try
-- to abstract a little bit away from that.

-- AL
-- DEFINE success
-- DEFINE failure
-- DEFINE orElse

-- REFACTOR makeChange as makeChange




------------------------------------------------------------------------------
-- Search tree vs. Search strategy

-- GA
-- In Haskell we can reify control flow; use:
-- DEFINE Success instead of success
-- DEFINE Failure instead of failure
-- DEFINE OrElse  instead of orElse



-- BUILD makeChange


-- DEFINE greedy
-- DEFINE firstChoice
-- DEFINE allChoices
-- DEFINE best (using an (a -> Int) measure)
