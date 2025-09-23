{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week05Live where

import Prelude hiding (Semigroup (..), Monoid (..), Foldable (..), Functor (..))
import Data.Char

-- Next week: 30th October: Class Test (24hrs, 50%).

-- TYPES TYPES TYPES

-- “Make Illegal States Unrepresentable”

-- “Billion Dollar Mistake”
--   NULL
--   (as invented by Sir Tony Hoare)
-- replace with Maybe (in Haskell)
--         with Optional<T> (in Java)

-- "Parse, Don't Validate"

newtype Metres = MkMetres Double
newtype Seconds = MkSeconds Double
newtype MetresPerSecond = MkMetresPerSecond Double

newtype Untrusted = MkUntrusted String

distanceToTheMoon :: Metres
distanceToTheMoon = MkMetres 34987394875

secondsInAnHour :: Seconds
secondsInAnHour = MkSeconds (60 * 60)

computeSpeed :: Metres -> Seconds -> MetresPerSecond
computeSpeed (MkMetres distance) (MkSeconds time) =
  MkMetresPerSecond (distance / time)

-- F# programming language from Microsoft
--   units of measure types built in


{- public class Student {
      // name is not null
      // at least one of dsUsername and registrationNumber is not null
      public final String name;
      public final String dsUsername;
      public final String registrationNumber;

      public Student(String name, String dsUsername) { .. }
      public Student(String name, String registrationNumber) { .. }
      public Student(String name, String dsUsername, String registrationNumber) { .. }
   }
-}

data These a b
  = MkThis a
  | MkThat b
  | MkThese a b

newtype DSUsername = MkDSUsername String
newtype RegistrationNumber = MkRegistrationNumber String

data Student = MkStudent
  { name    :: String
  , regInfo :: These DSUsername RegistrationNumber
  }

-- registration :: Student -> IO RegistrationNumber
student :: Student
student = MkStudent
  { name = "bob"
  , regInfo = MkThis (MkDSUsername "jjb15109")
  }

mkRegistrationNumber :: String -> Maybe RegistrationNumber
mkRegistrationNumber str
  | all isDigit str = Just (MkRegistrationNumber str)
  | otherwise = Nothing


-- module RegistrationNumber (RegistrationNumber, mkRegistrationNumber) where
--
-- newtype RegistrationNumber = MkRegistrationNumber String
-- mkRegistrationNumber :: String -> Maybe RegistrationNumber
-- mkRegistrationNumber = ...
--
-- getNumber :: RegistrationNumber -> String
-- getNumber (MkRegistrationNumber str) = str


class MyShow a where
  myshow :: a -> String

data Blah = A | B | C deriving Show

newtype CaseInsensitiveString =
  MkCIString String

instance Show CaseInsensitiveString where
  show (MkCIString str) = show (map toUpper str)

instance Eq CaseInsensitiveString where
  MkCIString str1 == MkCIString str2 =
    map toUpper str1 == map toUpper str2

-- Type class <~~~~~> interface in Java

-- public class JHGJH implements X, Y, Z

-- On Friday:
--  - Monoids -- generalising addition, multiplication, and, or, concatenation, ...
--  - Foldable, Functor

-- Semigroups
class Semigroup m where
  (<>) :: m -> m -> m

-- associativity : (x <> y) <> z == x <> (y <> z)

instance Semigroup [a] where
  (<>) = (++)

newtype Throwaway a = MkThrowaway { getThrowaway :: [a] } deriving Show

instance Semigroup (Throwaway a) where
  xs <> ys = MkThrowaway []

newtype Sum = MkSum { getSum :: Int } deriving Show

instance Semigroup Sum where
  MkSum m <> MkSum n = MkSum (m + n)

newtype Prod = MkProd { getProd :: Int } deriving Show

instance Semigroup Prod where
  MkProd m <> MkProd n = MkProd (m * n)

newtype Max = MkMax { getMax :: Int } deriving Show

instance Semigroup Max where
  MkMax m <> MkMax n = MkMax (max m n)

test :: Semigroup m => (Int -> m) -> m
test f = f 0 <> f 1 <> f 2

-- Monoids
class Semigroup m => Monoid m where
  mempty :: m

-- mempty <> x == x
-- x <> mempty == x

{- interface Monoid {
      -- binary method problem in OOP
   }
-}

instance Monoid [a] where
  mempty = []

-- instance Monoid (Throwaway a) where
--   mempty = < nothing sensible to write here >

instance Monoid Sum where
  mempty :: Sum
  mempty = MkSum 0

instance Monoid Prod where
  mempty = MkProd 1

-- instance Monoid Max where
--   mempty = -- no answer to go here
--     -- need to solve: mempty `max` x == x

-- Foldable
foldList :: Monoid m => [m] -> m
foldList []       = mempty
foldList (x : xs) = x <> foldList xs

newtype First a = MkFirst { getFirst :: Maybe a } deriving Show

instance Semigroup (First a) where
  MkFirst Nothing <> x = x
  x <> _ = x

instance Monoid (First a) where
  mempty = MkFirst Nothing


class Foldable t where
  fold :: Monoid m => t m -> m

instance Foldable [] where
  fold = foldList

instance Foldable Maybe where
  fold Nothing = mempty
  fold (Just m) = m

data Formula a
  = Atom a
  | IsTrue
  | And (Formula a) (Formula a)
  | Not (Formula a)
  deriving (Show)

instance Foldable Formula where
  fold (Atom m) = m
  fold IsTrue = mempty
  fold (And e f) = fold e <> fold f
  fold (Not e) = fold e

myFormula :: Formula String
myFormula = Not (And (Not (Atom "e")) (Atom "f"))

-- Formula String -> Formula [String]

mapFormula :: (a -> b) -> Formula a -> Formula b
mapFormula f (Atom a) = Atom (f a)
mapFormula f (And p q) = And (mapFormula f p) (mapFormula f q)
mapFormula f IsTrue = IsTrue
mapFormula f (Not p) = Not (mapFormula f p)

-- Functors
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Formula where
  fmap = mapFormula

instance Functor [] where
  fmap = map

getAll :: (Foldable t, Functor t) => t a -> [a]
getAll = fold . fmap (\x -> [x])

sumAll :: (Foldable t, Functor t) => t Int -> Int
sumAll = getSum . fold . fmap MkSum
