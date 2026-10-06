{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

type Coin = Amount
newtype Amount = MkAmount { getAmount :: Int }
-- Amounts should be >= 0
  deriving (Eq, Ord, Enum, Num)

instance Show Amount where
  show amt = show (getAmount amt)

instance Arbitrary Amount where
  arbitrary = fmap (MkAmount . abs) arbitrary

type Change = [Coin]
type Till = [Coin]

-- DISCUSS how Till, Change, Coin, Amount relate
-- (e.g. define a function turning X into Y)
tillTotal :: Till -> Amount
tillTotal = sum

changeAmount :: Change -> Amount
changeAmount = sum

validChange :: Maybe Change -> Amount -> Bool
validChange Nothing amt = False
validChange (Just chg) amt = changeAmount chg == amt

-- PONDER makeChange, a function that takes:
-- a till
-- an amount
-- and returns change matching the amount


-- WRITE some tests

-- 1. Unit tests
-- Till with exactly the right coin
-- Till with [1..10] and amount of 55

testVC :: Till -> Amount -> Bool
testVC tl amt = validChange (makeChange tl amt) amt

wholeTill :: Bool
wholeTill = testVC [1..10] (tillTotal [1..10])

lastCoin :: Bool
lastCoin = testVC [1..10] 10

-- 2. Property testing
-- What property do we expect the outcome to verify?

prop_VC :: Till -> Amount -> Property
prop_VC tl amnt =
  let mchng = makeChange tl amnt in
  isJust mchng ==> testVC tl amnt

-- Better test

allSubsets :: [a] -> [[a]]
allSubsets [] = [[]]
allSubsets (hd : tl) =
  let ih = allSubsets tl in
  map (\ subset -> hd : subset) ih ++ ih

-- Generates a lot of lists! To be used with
-- quickCheckWith (stdArgs {maxSize = 20}) prop_VC2
-- verboseCheckWith (stdArgs {maxSize = 20}) prop_VC2

prop_VC2 :: Till -> Amount -> Property
prop_VC2 tl amnt =
  let subsets = allSubsets tl in
  any (\ subTill -> changeAmount subTill == amnt) subsets
  ==> testVC tl amnt

-- DEFINE makeChange
makeChange :: Till -> Amount -> Maybe Change
makeChange tl amt = makeChangeAcc2 tl amt []

makeChangeAcc :: Till -> Amount -> Change -> Maybe Change
makeChangeAcc tl 0 hand = Just hand
makeChangeAcc [] amt hand = Nothing
makeChangeAcc (coin : tl) amt hand
  | coin > amt = makeChangeAcc tl amt hand
  | otherwise  = makeChangeAcc tl (amt - coin) (coin : hand)

makeChangeAcc2 :: Till -> Amount -> Change -> Maybe Change
makeChangeAcc2 tl 0 hand = Just hand
makeChangeAcc2 [] amt hand = Nothing
makeChangeAcc2 (coin : tl) amt hand
  | coin > amt = makeChangeAcc2 tl amt hand
  | otherwise  = case makeChangeAcc2 tl (amt - coin) (coin : hand) of
                      Just x -> Just x
                      Nothing -> (makeChangeAcc2 tl amt hand)


-- TEST makeChange
-- quickCheck, verboseCheck


-- FIX (?) makeChange


-- TEST new version
-- TEST with precondition (==>)
-- TEST with better inputs


-- REFACTOR (?) makeChange
