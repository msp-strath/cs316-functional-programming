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

prop_VC3 :: Till -> Bool
prop_VC3 tl =
  let subsets = allSubsets tl in
  all (\subTill -> testVC tl (changeAmount subTill)) subsets



-- DEFINE makeChange
makeChange :: Till -> Amount -> Maybe Change
makeChange tl amt = makeChangeAcc4 tl amt []

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
  | otherwise  =
    let keepCoin = makeChangeAcc2 tl (amt - coin) (coin : hand)  in
    case keepCoin of
      Just x -> Just x
      Nothing -> (makeChangeAcc2 tl amt hand)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing ma = ma
-- False & _ = False
-- True  & bool = bool -- x & 1 = x

andAlso :: Maybe a -> Maybe b -> Maybe (a, b)
andAlso Nothing _ = Nothing
andAlso (Just a) (Just b) = Just (a, b)
andAlso (Just a) Nothing = Nothing

makeChangeAcc3 :: Till -> Amount -> Change -> Maybe Change
makeChangeAcc3 tl 0 hand = Just hand
makeChangeAcc3 [] amt hand = Nothing
makeChangeAcc3 (coin : tl) amt hand
  | coin > amt = makeChangeAcc3 tl amt hand
  | otherwise  =
    let keepCoin = makeChangeAcc3 tl (amt - coin) (coin : hand) in
    let skipCoin = makeChangeAcc3 tl amt hand in
    keepCoin `orElse` skipCoin

-- TEST makeChange
-- quickCheck, verboseCheck

makeChangeAcc4 :: Till -> Amount -> Change -> Maybe Change
makeChangeAcc4 tl 0 hand = Just hand
makeChangeAcc4 [] amt hand = Nothing
makeChangeAcc4 (coin : tl) amt hand =
  let keepCoin = makeChangeAcc4 tl (amt - coin) (coin : hand) in
  let skipCoin = makeChangeAcc4 tl amt hand in
  if coin > amt then skipCoin else keepCoin `orElse` skipCoin


-- FIX (?) makeChange


-- TEST new version
-- TEST with precondition (==>)
-- TEST with better inputs


-- REFACTOR (?) makeChange



makeChangeAcc5 :: Till -> Amount -> Change -> [Change]
makeChangeAcc5 tl 0 hand = [hand]
makeChangeAcc5 [] amt hand = []
makeChangeAcc5 (coin : tl) amt hand =
  let keepCoin = makeChangeAcc5 tl (amt - coin) (coin : hand) in
  let skipCoin = makeChangeAcc5 tl amt hand in
  if coin > amt then skipCoin else keepCoin `orAlso` skipCoin

orAlso :: [a] -> [a] -> [a]
orAlso ansl ansr = ansl ++ ansr

makeAllChanges :: Till -> Amount -> [Change]
makeAllChanges tl amt = makeChangeAcc5 tl amt []


data Choices a
  = Value a
  | Oops
  | Branch (Choices a) (Choices a)


makeChangeAcc6 :: Till -> Amount -> Change -> Choices Change
makeChangeAcc6 tl 0 hand = Value hand
makeChangeAcc6 [] amt hand = Oops
makeChangeAcc6 (coin : tl) amt hand =
  let keepCoin = makeChangeAcc6 tl (amt - coin) (coin : hand) in
  let skipCoin = makeChangeAcc6 tl amt hand in
  if coin > amt then skipCoin else Branch keepCoin skipCoin

greedy :: Choices a -> Maybe a
greedy (Value a) = Just a
greedy Oops = Nothing
greedy (Branch l r) = greedy l

backtracking :: Choices a -> Maybe a
backtracking (Value a) = Just a
backtracking Oops = Nothing
backtracking (Branch l r) = backtracking l `orElse` backtracking r

allValues :: Choices a -> [a]
allValues (Value a) = [a]
allValues Oops = []
allValues (Branch l r) = allValues l `orAlso` allValues r
