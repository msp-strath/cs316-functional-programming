{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week04Live where

import Prelude hiding (foldl, length, product, sum, concat)

-- WEEK 04 : PATTERNS of RECURSION

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : (append xs ys)

append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

common :: b -> (a -> b -> b) -> [a] -> b
common base step [] = base
common base step (x : xs) = step x (common base step xs)

sum' :: [Int] -> Int
sum' = foldr (+) 0

-- a : (b : (c : (d : [])))
-- a + (b + (c + (d +  0)))

concat' :: [[a]] -> [a]
concat' = foldr (++) []

data Natural = Zero | Succ Natural deriving (Show)

foldNatural :: b          -- ^ base case
            -> (b -> b)   -- ^ step case
            -> Natural -> b  -- ^ a machine for crushing naturals
foldNatural base step Zero     = base
foldNatural base step (Succ n) = step (foldNatural base step n)

add :: Natural -> Natural -> Natural
add x y = foldNatural y Succ x

add' :: Natural -> Natural -> Natural
add' Zero     y = y
add' (Succ x) y = Succ (add' x y)

-- x = Succ (Succ (Succ Zero))     "3"
-- y = Succ (Succ Zero)            "2"
--
--   Succ (Succ (Succ Zero))
--                    (Succ (Succ Zero))
--              (Succ (Succ (Succ Zero)))
--        (Succ (Succ (Succ (Succ Zero))))
--  (Succ (Succ (Succ (Succ (Succ Zero)))))

-- a : (b : (c : []))
-- a + (b + (c + 0 ))
-- (((0 + a) + b) + c)

foldl :: b -- initial value of the accumulator
      -> (b -> a -> b) -- update function for the accumulator
      -> [a] -> b -- list-crushing function
foldl acc update [] = acc
foldl acc update (x : xs) =
  let acc' = update acc x in
  foldl acc' update xs

step :: String -> String -> String
step a b = "(" ++ a ++ " - " ++ b ++ ")"

base :: String
base = "0"


data Bank = Account Integer Integer deriving Show

data Transaction
  = CreditA Integer
  | CreditB Integer
  | DebitA Integer
  | DebitB Integer
  | TransferAtoB Integer
  deriving Show

bankStep :: Bank -> Transaction -> Bank
bankStep (Account a b) (CreditA amount)      = Account (a + amount) b
bankStep (Account a b) (DebitA amount)       = Account (a - amount) b
bankStep (Account a b) (CreditB amount)      = Account a (b + amount)
bankStep (Account a b) (DebitB amount)       = Account a (b - amount)
bankStep (Account a b) (TransferAtoB amount) = Account (a - amount) (b + amount)

daysTransactions :: [Transaction]
daysTransactions =
  [ CreditA 10,
    DebitB  20,
    CreditA 10,
    TransferAtoB 20
  ]

initialBank :: Bank
initialBank = Account 0 0
