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

data Bank = Account Integer Integer
 deriving Show



-- DEFINE Transaction
data Transaction = CreditA Integer
                  | DebitA Integer
                  | CreditB Integer
                  | DebitB Integer
                  | TransferAtoB Integer
                  | DoNothing
  deriving Show

step :: Bank -> Transaction -> Bank
step (Account a b) (CreditA x) = Account (x+a) b
step (Account a b) (DebitA x)  = Account (a - x) b
step (Account a b) (CreditB x) = Account a (b + x)
step (Account a b) (DebitB x)  = Account a (b - x)
step (Account a b) (TransferAtoB x) = Account (a - x) (b + x)
step (Account a b) DoNothing   = Account a b


-- DEFINE the semantics of transaction as a Bank-transformer

-- DEFINE an example of a list of transactions
dayTransaction :: [Transaction]
dayTransaction = [DebitB 40, DoNothing, TransferAtoB 40, DebitB 40]
-- DEFINE an initial bank


initialBalance :: Bank
initialBalance = Account 0 0




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

data Formula a
  = Atom a
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or  (Formula a) (Formula a)
  deriving (Show)

-- DEFINE an example formula
myFormula :: Formula String
myFormula = Not (And (Atom "X") (Atom "Y"))


-- DEFINE foldrFormula (same type as for lists) for formulas

foldrFormula :: (a -> b -> b) -> b -> Formula a -> b
foldrFormula c n f = case f of
  Atom a -> c a n
  Not f1 -> foldrFormula c n f1
  And f1 f2 -> let n2 = foldrFormula c n f2 in
               foldrFormula c n2 f1
  Or f1 f2 -> let n2 = foldrFormula c n f2 in
               foldrFormula c n2 f1

mapFormula :: (a -> b) -> Formula a -> Formula b
mapFormula = undefined

allAtoms :: Formula a -> [a]
allAtoms = foldrFormula (:) []


-- DISCUSS lack of control
-- DEFINE general foldFormula


-- DEFINE example (e.g. 'simplify' pushing negations inwards)

-- DISCUSS a property linking foldr, foldrFormula, and foldFormula

data FormulaShape a r
  = AtomShape a
  | NotShape r
  | AndShape r r
  | OrShape r r
  deriving (Show)

data RoseTree a = MkRose a ([RoseTree a])

data FullFormula a = MkFull (FormulaShape a (FullFormula a))

mapFormulaShape :: (r -> s) -> FormulaShape a r -> FormulaShape a s
mapFormulaShape f shp = case shp of
  AtomShape a -> AtomShape a
  NotShape shp1 -> NotShape (f shp1)
  AndShape shp1 shp2 -> AndShape (f shp1) (f shp2)
  OrShape shp1 shp2 -> OrShape (f shp1) (f shp2)

fold :: (FormulaShape a b -> b) -> (FullFormula a -> b)
fold alg (MkFull shp) = alg (mapFormulaShape (fold alg) shp)


nOt :: FullFormula a -> FullFormula a
nOt = MkFull . NotShape

aNd :: FullFormula a -> FullFormula a -> FullFormula a
aNd f g = MkFull (AndShape f g)

oRr :: FullFormula a -> FullFormula a -> FullFormula a
oRr f g = MkFull (OrShape f g)

simplify :: FullFormula a -> Bool -> FullFormula a
simplify = fold alg where

  alg :: FormulaShape a (Bool -> FullFormula a)
      ->                (Bool -> FullFormula a)
  alg (AtomShape a) b = (if b then nOt else id) (MkFull (AtomShape a))
  alg (NotShape bf) b = bf (not b)
  alg (AndShape bf1 bf2) b = (if b then oRr else aNd) (bf1 b) (bf2 b)
  alg (OrShape bf1 bf2) b = (if b then aNd else oRr) (bf1 b) (bf2 b)
