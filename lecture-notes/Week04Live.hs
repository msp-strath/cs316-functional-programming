{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week04Live where

import Data.Char (toUpper)
import Prelude hiding (foldl, length, product, sum, concat)


import Test.QuickCheck

-- WEEK 04 : PATTERNS of RECURSION

------------------------------------------------------------------------
-- Foldr

-- DEFINE sum
-- sum :: [Int] -> Int

-- DEFINE append
-- append :: [a] -> [a] -> [a]

-- DEFINE concat
-- concat :: [[a]] -> [a]


-- They all share a similar structure,
-- DEFINE foldrList




-- DISCUSS symbolic execution

-- OBSERVE behaviour via
-- foldrTracing :: Show a => [a] -> String
-- foldrTracing = foldrList "base" (\ x str -> show x ++ " `step` (" ++ str ++ ")")

-- DEFINE sum'
-- DEFINE append'
-- DEFINE concat'





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
