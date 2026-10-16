{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
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
-- 1     :  (2   :  (3   :   []))
-- 1 `c` (2 `c` (3 `c` (n)))

-- 1     :  (2   :  (3   :   []))
-- (2*1) :  (2*2 :  (2*3 :   []))
-- (2*1) `c`(2*2 `c`(2*3 `c` (n)))


--foldrList :: (a -> b -> b) -> b -> [a] -> b
-- map f  (1 : (  2 : (  3 : [])))
--      (f 1 : (f 2 : (f 3 : [])))
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldrList (\head acc -> (f head) : acc) [] xs

-- Foldr helps us structure our code and automatically
-- benefit from universal laws.

-- What is `foldr c n (xs ++ ys)`?

prop_foldrAppend :: [Int] -> [Int] -> Bool
prop_foldrAppend xs ys =
  foldr (-) 0 (xs ++ ys)
  == foldr (-) (foldr (-) 0 ys) xs

-- What is `foldr c n (map f xs)`?

prop_foldrMap :: [Int] -> Bool
prop_foldrMap xs =
  foldr (-) 0 (map (2*) xs)
  == foldr (\ x r -> (2*x) - r) 0 xs


------------------------------------------------------------------------
-- Generalising fold

-- DEFINE data Natural
-- data Natural = Zero | Succ Natural
data Natural where
  Zero :: {- no arg -} Natural
  Succ :: Natural   -> Natural
  deriving (Show)

-- DEFINE one
one :: Natural
one = Succ Zero

-- DEFINE two
two :: Natural
two = Succ one

-- foldrList
--    (:) : a -> [a] -> [a]
--
-- :: (a -> b -> b) -- A function taking a head and
--                  -- an already transformed tail and returning a b
--
--    [] : [a]
-- -> b             -- A b standing for the meaning of []
--
-- -> [a] -> b      -- To Transform lists of as in to bs


-- DEFINE foldNatural
foldNatural
  :: b
  -> (b -> b)
  -> Natural -> b
foldNatural z s Zero = z
foldNatural z s (Succ n) = s (foldNatural z s n)


-- DEFINE add
-- DEFINE add'
add :: Natural -> Natural -> Natural
add        m  n  = foldNatural    n  Succ m
-- append' xs ys = foldrList  (:) ys   |  xs
--                             ^-------'

-- DISCUSS addition vs. append

------------------------------------------------------------------------
-- Foldl

sumLeft :: [Int] -> Int
sumLeft xs = go 0 xs
  where
     go n []     = n
     go n (x:xs) = go (n+x) xs
-- go :: Int -> [Int] -> Int
-- go   Accumulator -> list -> value

foldlist :: b -> (b -> a -> b) -> [a] -> b
foldlist acc f [] = acc
foldlist acc f (x:xs) = foldlist (f acc x) f xs

sumLeft' xs = foldlist 0 (+) xs


-- REMEMBER sum
-- DEFINE sumAcc

-- DEFINE foldlList


-- DISCUSS symbolic execution

-- OBSERVE the behaviour using
foldltracing :: Show a => [a] -> String
foldltracing = foldlist "acc" (\ str x -> "(" ++ str ++ ") `upd` " ++ show x)





-- Example
-- foldl for state-changing operation

-- A Bank with a grand total of two clients

-- DEFINE data Bank

data Bank = Account Int Int
 deriving Show



-- DEFINE Transaction
data Transaction = CreditA Int
   | CreditB Int
   | DebitA Int
   | DebitB Int
   | TransferAtoB Int
   | DoNothing
 deriving Show



-- DEFINE the semantics of transaction as a Bank-transformer
--      b    -> a           -> b
step :: Bank -> Transaction -> Bank
step (Account a b) (CreditA money) = Account (a+money) b
step (Account a b) (CreditB money) = Account a       (b+money)
step (Account a b) (DebitA x)      = Account (a-x) b
step (Account a b) (DebitB x)      = Account a     (b -x)
step (Account a b) (TransferAtoB x)= Account (a-x) (b+x)
step (Account a b) DoNothing       = Account a b

-- DEFINE an example of a list of transactions
dayTransaction :: [Transaction]
dayTransaction = [CreditA 50, TransferAtoB 12, DoNothing, TransferAtoB 12, CreditA 12]

-- DEFINE an initial bank
initialBalance :: Bank
initialBalance = Account 0 0


-- EXPERIMENT with running the transactions
runTransaction = foldlist initialBalance step dayTransaction


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
