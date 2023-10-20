{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week05Intro where

import Prelude hiding (Left, Right, Semigroup (..),
                       Foldable (..), Functor (..),
                       Monoid (..), Maybe (..))
import Data.Char

{-    WEEK 05 : CLASSES OF TYPES

   1. Types in Haskell
   2. The Billion-dollar mistake
   3. Make Illegal States Unrepresentable
   4. Parse; don't validate
   5. Introduction to Type Classes

-}










-- 1. Type Synonyms, Newtypes and Data types

-- Giving things names
data Direction = Up | Down | Left | Right
-- data Tree a = Leaf | Node (Tree a) a (Tree a)

type Transformation = Direction -> Direction

flipVertically :: Transformation
               -- Direction -> Direction
flipVertically Up   = Down
flipVertically Down = Up
flipVertically d    = d

newtype Bool2 = MkBool2 Bool
-- data Bool2 = MkBool2 Bool

-- Units of Measure
-- Int, Double, Integer, Float

newtype Metres = M Double
newtype Seconds = S Double

distanceToTheMoon :: Metres
distanceToTheMoon = M 38275628376

timeForLightToTheMoon :: Seconds
timeForLightToTheMoon = S 49

-- Double<m/s>


-- Passwords

newtype Password = P String



-- 2. Exceptional conditions as Data

--  Maybe vs null
--  Maybe vs exceptions

-- Billion dollar mistake:
-- https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare/

data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Show)

-- case maybeThing of
--   Nothing -> ...
--   Just a -> ...
--
--  a.doThing()

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []           = Nothing
search k ((k',v):kvs) = if k == k' then Just v else search k kvs

-- [(k,Maybe Double)]   -> Maybe (Maybe Double)
-- [(k,Double)]         -> Maybe Double


-- https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Map.html#get(java.lang.Object)


-- 3. Make Illegal States unrepresentable
--      https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/

{-
        public class Student {
           // never null!
           @Nonnull
           private String name;

           // at least one of these is non-null
           private String registrationNumber;

           private String dsUsername;

           // ...
        }
-}

data IdInfo
  = OnlyRegnum String
  | OnlyUsername String
  | Both String String
  deriving Show

data Student = MkStudent { name :: String
                         , idinfo :: IdInfo
                         } deriving Show



-- Non-empty lists

-- [] , [1,2,3]

data NEList a = NEList a [a]

-- head :: NEList a -> a
-- head (NEList a _) = a

-- head :: [a] -> Maybe a



-- Parse; don't Validate
--    https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

validateUsername :: String -> Bool
validateUsername "" = False
validateUsername _  = True

parseUsername :: String -> Maybe (NEList Char)
parseUsername ""     = Nothing
parseUsername (x:xs) = Just (NEList x xs)

type Record = [(String,String)]

checkRecord :: Record -> Bool
checkRecord record =
  case search "username" record of
    Nothing -> False
    Just _ ->
      case search "id" record of
        Nothing -> False
        Just _ -> True

parseRecord :: Record -> Maybe (String,String)
parseRecord record =
  case search "username" record of
    Nothing -> Nothing
    Just username ->
      case search "id" record of
        Nothing -> Nothing
        Just id -> Just (username, id)


-- REMINDER:
--
--   *** CLASS TEST ***
--
--  12:00 noon Wednesday 25th Oct to 12:00 noon Thursday 26th Oct
--
--      Covering weeks 1-5.
--      Worth 50% of course mark
--      Redemption test in Week 9




-- 4. Type classes

-- Eq, Show, Ord


--   ==  : String -> String -> Bool
--       : Int -> Int -> Bool
--       : Double -> Double -> Bool
--       : Bool -> Bool -> Bool
--       : List String -> List String -> Bool
--  ===      (type and value comparison in JavaScript)
--
--   +  : Int -> Int -> Int
--      : Double -> Double -> Double
--      : BigInteger -> BigInteger -> BigInteger

-- boolean equals(Object o)
-- String toString()

--  x.toString()

-- eqString
-- eqInt
-- eqDouble
-- eqBool

-- addInt
-- addDouble
-- addBigInteger

-- class Eq a where

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
--  deriving (Eq)

{-
class Eq a where
  (==) :: a -> a -> Bool
-}

instance Eq (Tree a) where
  (==) Leaf Leaf = True
  (==) (Node l1 _ r1) (Node l2 _ r2) = l1 == l2 && r1 == r2
  (==) _ _ = False

{-
class Show a where
  show :: a -> String
-}

instance Show (Tree a) where
  show Leaf = "Leaf"
  show (Node l _ r) = "(Node " ++ show l ++ " _ " ++ show r ++ ")"


{- Type classes:

   - define a common interface that can be implemented by many types

   - For example:
       - Eq    for equality testing
       - Show  for printing
       - Ord   for ordering
       - Num   for numeric types


-}

-- Automatic differentiation via dual numbers:
data Dual = Dual { primal :: Double,
                   deriv  :: Double
                 } deriving Show

-- (*) (Dual p1 d1) (Dual p2 p2) = Dual (p1 * p2) (p1 * d2 + p2 * d1)



--- Next time: Semigroups, Monoids, Foldable, Functor

class Semigroup a where
  (<>) :: a -> a -> a

  -- Associativity:
  -- forall a b c. a <> (b <> c) == (a <> b) <> c

instance Semigroup Integer where
  (<>) = (+)
  -- (<>) = (*)
  -- (<>) = max
  -- (<>) = min

instance Semigroup Bool where
  (<>) = (&&)
  -- (<>) = (||)

instance Semigroup [a] where
  (<>) = (++)

data RoughCount = Zero | One | Many deriving (Show, Eq)

instance Semigroup RoughCount where
  Zero <> x    = x
  x    <> Zero = x
  One  <> One  = Many
  Many <> x    = Many
  x    <> Many = Many

data RockPaperScissors = Rock | Paper | Scissors deriving (Eq, Show)

play :: RockPaperScissors -> RockPaperScissors -> RockPaperScissors
play Rock     Scissors = Rock
play Paper    Rock     = Paper
play Scissors Paper    = Scissors
play Rock     Rock     = Rock
play Paper    Paper    = Paper
play Scissors Scissors = Scissors
play x        y        = play y x

{- NOT ASSOCIATIVE:

ghci> play Rock (play Paper Scissors)
Rock
ghci> play (play Rock Paper) Scissors
Scissors
-}

class Semigroup a => Monoid a where
  mempty :: a
  -- forall a. a <> mempty == a
  -- forall a. mempty <> a == a

instance Monoid Integer where
  -- (+)
  mempty = 0
  -- NO MONOID for max/min

instance Monoid Bool where
  -- (&&)
  mempty = True

instance Monoid [a] where
  mempty = []

foldList :: Monoid a => [a] -> a
foldList []     = mempty
foldList (x:xs) = x <> foldList xs

foldTree :: Monoid a => Tree a -> a
foldTree Leaf = mempty
foldTree (Node l x r) = foldTree l <> x <> foldTree r

foldMaybe :: Monoid a => Maybe a -> a
foldMaybe Nothing  = mempty
foldMaybe (Just a) = a

class Foldable c where
  fold :: Monoid a => c a -> a

instance Foldable [] where
  fold = foldList

instance Foldable Tree where
  fold = foldTree

instance Foldable Maybe where
  fold = foldMaybe

-- Functors

-- map      :: (a -> b) -> [a] -> [b]
-- mapTree  :: (a -> b) -> Tree a -> Tree b
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b

foldMap :: Monoid b => (a -> b) -> [a] -> b
foldMap f = fold . map f

class Functor c {- Mappable c -} where
  fmap :: (a -> b) -> c a -> c b
