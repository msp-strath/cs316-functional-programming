{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week04Live where

import Data.Char (toUpper)
import Prelude hiding (foldl, length, product, sum, concat)


import Test.QuickCheck

-- WEEK 04 : PATTERNS of RECURSION

------------------------------------------------------------------------
-- Foldr

-- DEFINE sum
-- DEFINE append
-- DEFINE concat


sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

sum'  = common 0 (+)

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

append' :: [a] -> [a] -> [a]
append' xs ys = common ys (:) xs

{-
prop_append' :: [Int] -> [Int] -> Bool
prop_append' xs ys = append xs ys == append' xs ys
-}

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss
--                 a a ->a     [a]

common :: b               -- base case
       -> (a -> (b -> b)) -- from a to change of b-state
       -> [a]
       -> b
common base step [] = base
common base step (x:xs) =  x `step` (common base step xs)

-- DEFINE common


-- DISCUSS
-- Symbolic execution on
-- a    :   (b    :   (c   :    (d : [])))
-- a `step` (b `step` (c `step` (d `step` base)))

commonTracing :: Show a => [a] -> String
commonTracing = common "base" (\ x str -> show x ++ " `step` (" ++ str ++ ")")

-- DEFINE sum'
-- DEFINE append'
-- DEFINE concat'



-- DEFINE foldRtracing


------------------------------------------------------------------------
-- Generalising fold

-- DEFINE data Natural

data Natural where
  Zero :: Natural
  Succ :: Natural -> Natural
  deriving (Show)

one :: Natural
one = Succ Zero

two :: Natural
two = Succ (Succ Zero)

-- DEFINE foldNatural
foldNatural
  :: b          -- base : P Zero
  -> (b -> b)   -- step : ∀ n → P n → P (Succ n)
  -> Natural    -- ∀ n.
  -> b          -- P n
foldNatural zero succ Zero = zero
foldNatural zero succ (Succ n) = succ (foldNatural zero succ n)


-- DEFINE add
-- DEFINE add'

add :: Natural -> Natural -> Natural
add m n = foldNatural m Succ n


-- DISCUSS addition
-- x = Succ (Succ (Succ Zero))
-- y = Succ (Succ Zero)

--
-- a : (b : (c : []))
-- a + (b + (c + 0 ))
-- (((0 + a) + b) + c)



------------------------------------------------------------------------
-- Foldl

-- DEFINE foldl

{-
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
-}

sumAcc :: Int -> [Int] -> Int
sumAcc acc [] = acc
sumAcc acc (x:xs) = sumAcc (acc+x) xs

sum'' :: [Int] -> Int
sum'' = sumAcc 0

nommoc
  :: b             -- initial accumulator
  -> (b -> a -> b) -- accumulator update
  -> [a]
  -> b
nommoc acc upd [] = acc
nommoc acc upd (x:xs) = nommoc (acc `upd` x) upd xs

nommoctracing :: Show a => [a] -> String
nommoctracing = nommoc "acc" (\ str x -> "(" ++ str ++ ") `upd` " ++ show x)

-- ((((acc `upd` 1) `upd` 2) `upd` 3) `upd` 4) `upd` 5
--               1    :   2    :   3    :   4    :   5  : []

-- DEFINE foldltracing

-- foldl
-- foldl'









-- Example
-- foldl for state-changing operation

-- A Bank with a grand total of two clients

-- DEFINE data Bank

-- DEFINE Transaction


-- DEFINE the semantics of transaction as a Bank-transformer

-- DEFINE an example of a list of transactions

-- DEFINE an initial bank




------------------------------------------------------------------------------
-- Generalising the Fold pattern
--

-- interface ListVisitor<Elements, Result> {
--    public Result visitNil();
--    public Result visitElement(Element a, Result restOfTheList);
-- }
--
-- Result visitListRight(ListVisitor<Element, Result> visitor, List<Element> list) {
--    Result answer = visitor.visitNil();
--    for (int i = list.length() - 1; i >= 0; i--) {
--       answer = visitor.visitElement(list.get(i), answer);
--    }
--    return answer;
-- }
--
-- Result visitListLeft(ListVisitor<Element, Result> visitor, List<Element> list) {
--    Result answer = visitor.visitNil();
--    for (Element e : list) {
--       answer = visitor.visitElement(e, answer);
--    }
--    return answer;
-- }

------------------------------------------------------------------------------
--

-- DEFINE (Formula a)

-- DEFINE an example formula
myFormula :: Formula String
myFormula = Not (And (Atom "X") (Atom "Y"))


-- DEFINE foldrFormula (same type as for lists) for formulas


-- DISCUSS lack of control
-- DEFINE general foldFormula


-- DEFINE example (e.g. 'simplify' pushing negations inwards)

-- DISCUSS a property linking foldr, foldrFormula, and foldFormula
