{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week04Live where

import Data.Char (toUpper)
import Prelude hiding (foldl, length, product, sum, concat)


import Test.QuickCheck

-- WEEK 04 : PATTERNS of RECURSION

------------------------------------------------------------------------
-- Foldr

-- DEFINE sum
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

-- DEFINE append
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x:(append xs ys)


-- DEFINE concat
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ (concat xss)

-- They all share a similar structure,
-- DEFINE foldrList
foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList c n [] = n
foldrList c n (x:xs) = x `c` (foldrList c n xs)

-- DEFINE sum'
-- DEFINE append'
-- DEFINE concat'

sum'          = foldrList (+) 0
append' xs ys = foldrList (:) ys xs
concat'       = foldrList (++) []

-- DISCUSS symbolic execution

-- OBSERVE behaviour via
foldrTracing :: Show a => [a] -> String
foldrTracing = foldrList (\ x str -> show x ++ " `c` (" ++ str ++ ")") "n"

-- [1,2,3]
-- 1  :  (2  :  (3  :   []))
-- 1 `c` (2 `c` (3 `c` (n)))
--foldrList :: (a -> b -> b) -> b -> [a] -> b
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldrList (\head acc -> (f head) : acc) [] xs

-- Foldr helps us structure our code and automatically
-- benefit from universal laws.

-- What is `foldr c n (xs ++ ys)`?
-- What is `foldr c n (map f xs)`?




------------------------------------------------------------------------
-- Generalising fold

-- DEFINE data Natural
-- data Natural

-- DEFINE one
-- one :: Natural

-- DEFINE two
-- two :: Natural

-- DEFINE foldNatural


-- DEFINE add
-- DEFINE add'


-- DISCUSS addition vs. append

------------------------------------------------------------------------
-- Foldl

-- REMEMBER sum
-- DEFINE sumAcc

-- DEFINE foldlList


-- DISCUSS symbolic execution

-- OBSERVE the behaviour using
-- foldltracing :: Show a => [a] -> String
-- foldltracing = foldlList "acc" (\ str x -> "(" ++ str ++ ") `upd` " ++ show x)





-- Example
-- foldl for state-changing operation

-- A Bank with a grand total of two clients

-- DEFINE data Bank

-- data Bank


-- DEFINE Transaction
-- data Transaction

-- DEFINE the semantics of transaction as a Bank-transformer
-- step :: Bank -> Transaction -> Bank


-- DEFINE an example of a list of transactions
-- dayTransaction :: [Transaction]


-- DEFINE an initial bank
-- initialBalance :: Bank


-- EXPERIMENT with running the transactions



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

-- data Formula a

-- DEFINE an example formula
-- myFormula :: Formula String


-- DEFINE foldrFormula (same type as for lists) for formulas
-- foldrFormula :: (a -> b -> b) -> b -> Formula a -> b


-- DEFINE allAtoms using foldrFormula
-- allAtoms :: Formula a -> [a]


-- DEFINE mapFormula using foldrFormula
-- mapFormula :: (a -> b) -> Formula a -> Formula b


-- DISCUSS lack of control
-- DEFINE general foldFormula


-- DEFINE example (e.g. 'simplify' pushing negations inwards)

-- DISCUSS a property linking foldr, foldrFormula, and foldFormula


------------------------------------------------------------------------------
-- The generic recipe: using the algebra over a base functor

-- data FormulaShape a r


-- mapFormulaShape :: (r -> s) -> FormulaShape a r -> FormulaShape a s
-- fold :: (FormulaShape a b -> b) -> (FullFormula a -> b)


-- DEFINE pseudo constructors
-- mkNot :: FullFormula a -> FullFormula a

-- mkAnd :: FullFormula a -> FullFormula a -> FullFormula a

-- mkOr :: FullFormula a -> FullFormula a -> FullFormula a


-- DEFINE simplification as an algebra
-- simplify :: FullFormula a -> Bool -> FullFormula a
