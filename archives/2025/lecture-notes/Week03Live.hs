{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Live where

import Data.List (subsequences)
import Data.Maybe
import Test.QuickCheck (Property, (==>))

------------------------------------------------------------------------
-- Coming (sooner or later)

-- AL
-- Class test

-- GA
-- Coursework

















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

cleanup :: (Till -> Amount -> a) -> (Till -> Amount -> a)
cleanup f till amount = f (map abs till) (abs amount)

prop_makeChange1 :: Till -> Amount -> Property
prop_makeChange1 = cleanup $ \ till amount ->
  let res = makeChange till amount [] in
  isJust res ==> changeTotal (fromMaybe [] res) == amount

-- DEFINE makeChange

makeChange :: Till -> Amount -> Change -> Maybe Change
makeChange _ 0 acc = Just (reverse acc)
makeChange (coin:till) n acc
  | n >= coin = makeChange till (n-coin) (coin:acc)
  | otherwise = makeChange till n acc
makeChange [] n acc = Nothing


makeChange2 :: Till -> Amount -> Change -> Maybe Change
makeChange2 _ 0 acc = Just (reverse acc)
makeChange2 [] n acc = Nothing
makeChange2 (coin:available) n acc
      | n >= coin =
        let future = makeChange2 available (n - coin) (coin:acc) in
        case future of
          Just x -> Just x
          Nothing -> (makeChange2 available n acc)
      | otherwise = makeChange2 available n acc

{-
-- case, defined by hand
makeChangeCase
  :: Till -> Amount -> Change
  -> Maybe Change
  -> Maybe Change
makeChange (coin:available) n acc (Just x) = Just x
makeChange (coin:available) n acc Nothing = makeChange available n acc
-}

-- GA
-- property test
-- "if there is a valid subset of coins in the till, we successfully get change"

prop_makeChange2 :: Till -> Amount -> Bool
prop_makeChange2 = cleanup $ \ till amount ->
  let candidates = subsequences till in
  any (\ chg -> changeTotal chg == amount) candidates
  == isJust (makeChange till amount [])


-- AL
-- DISCUSS flaw
-- FIX makeChange


makeChange3 :: Till -> Amount -> Change -> Maybe Change
makeChange3 _ 0 acc = Just (reverse acc)
makeChange3 [] n acc = Nothing
makeChange3 (coin:available) n acc
      | n >= coin =
        makeChange3 available (n - coin) (coin:acc)
        `orElse`
        makeChange3 available n acc
      | otherwise = makeChange3 available n acc



------------------------------------------------------------------------------
-- The Domain Specific Language of Failure

-- Instead of doing case analysis, using Maybe, Nothing, Just, let's try
-- to abstract a little bit away from that.

-- AL
-- DEFINE success
-- DEFINE failure
-- DEFINE orElse

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just v) _ = Just v
orElse Nothing ma = ma

-- REFACTOR makeChange as makeChange




------------------------------------------------------------------------------
-- Search tree vs. Search strategy

-- GA
-- In Haskell we can reify control flow; use:
-- DEFINE Success instead of success
-- DEFINE Failure instead of failure
-- DEFINE OrElse  instead of orElse


data Choices a
  = Success a
  | Failure
  | OrElse (Choices a) (Choices a)
  deriving (Show)
{-
makeChange3 :: Till -> Amount -> Change -> Maybe Change
makeChange3 _ 0 acc = Just (reverse acc)
makeChange3 [] n acc = Nothing
makeChange3 (coin:available) n acc
      | n >= coin =
        makeChange3 available (n - coin) (coin:acc) in
        `orElse`
        makeChange3 available n acc
      | otherwise = makeChange3 available n acc


-}
makeChange4 :: Till -> Amount -> Change -> Choices Change
makeChange4 _ 0 acc = Success (reverse acc)
makeChange4 [] _ _ = Failure
makeChange4 (coin:available) n acc
  | n >= coin =
    OrElse (makeChange4 available (n - coin) (coin:acc))
           (makeChange4 available n acc)
  | otherwise = makeChange4 available n acc

-- BUILD makeChange


-- DEFINE greedy

greedy :: Choices a -> Maybe a
greedy Failure = Nothing
greedy (Success v) = Just v
greedy (OrElse l _) = greedy l

backtracking :: Choices a -> Maybe a
backtracking Failure = Nothing
backtracking (Success v) = Just v
backtracking (OrElse l r) =
  backtracking l `orElse` backtracking r

-- DEFINE allChoices
-- DEFINE best (using an (a -> Int) measure)
