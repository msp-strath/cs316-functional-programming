{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week04Live where

import Data.Char (toUpper)
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
step a b = "(" ++ a ++ " ### " ++ b ++ ")"

{-
   public static String step (String a, String b) {
     return "(" + a + " + " + b + ")";
   }
-}

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

------------------------------------------------------------------------------

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

data Formula a
  = Atom a
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Not (Formula a)
  deriving Show

foldrFormula :: (a -> b -> b) -> b -> Formula a -> b
foldrFormula combine initial f = case f of
  Atom a  -> combine a initial
  And e f ->
    -- foldr c n (xs ++ ys) == foldr c (foldr c n ys) xs
    let intermediate = foldrFormula combine initial f in
    let final        = foldrFormula combine intermediate e in
    final
  Or e f ->
    -- foldr c n (xs ++ ys) == foldr c (foldr c n ys) xs
    -- f x y (e a b) == let z = e a b in f x y z
    let intermediate = foldrFormula combine initial f in
    let final        = foldrFormula combine intermediate e in
    final
  Not f -> foldrFormula combine initial f

myFormula :: Formula String
myFormula = Not (And (Atom "X") (Atom "Y"))

foldFormula :: (a -> result) -- atoms
            -> (result -> result -> result) -- ands
            -> (result -> result -> result) -- ors
            -> (result -> result) -- not
            -> Formula a
            -> result
foldFormula atom and or not (Atom b)  = atom b
foldFormula atom and or not (And e f) =
  and (foldFormula atom and or not e)
      (foldFormula atom and or not f)
foldFormula atom and or not (Or  e f) =
  or (foldFormula atom and or not e)
     (foldFormula atom and or not f)
foldFormula atom and or not (Not f)   =
  not (foldFormula atom and or not f)

-- foldrFormula c n = foldr c n . foldFormula (\x -> [x]) (++) (++) id

------------------------------------------------------------------------------
-- List comprehensions

exampleList :: [Int]
exampleList = [1..10]

evens :: [Int]
evens = [  x     | x <- exampleList
                 , x `mod` 2 == 0
                 ]

evenSums :: [(Int, Int)]
evenSums =
  [ (x, y)
  | x <- exampleList, y <- exampleList
  , (x + y) `mod` 2 == 0
  , x <= y ]

-- SELECT DISTINCT T1.x, T2.x
-- FROM exampleList as T1, exampleList as T2
-- WHERE (T1.x + T2.x) `mod` 2 == 0
--   AND T1.x <= T2.x

myDatabase :: [(String, String, Int)]
myDatabase = [ ("BobTown",        "Mars",    100)
             , ("GallaisVille",   "Venus",   200)
             , ("Alasdairopolis", "Mercury", 1)
             , ("JulesCity",      "Mars",    200)
             ]

-- An example "query"
myQuery = [ map toUpper cityName
          | (cityName, planet, pop) <- myDatabase
          , (pop > 5) || (planet == "Mars")
          ]

-- equivalent to
--
