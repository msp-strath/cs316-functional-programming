{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week05Live where

import Prelude hiding (Semigroup (..), Monoid (..), Foldable (..), Functor (..))
import Data.Char

import Test.QuickCheck

-- Next week: 28th October: Class Test (1-3pm LT1201, 50%).

------------------------------------------------------------------------
-- TYPES TYPES TYPES

-- “Make Illegal States Unrepresentable” (OCaml / Jane Street)

-- “Billion Dollar Mistake”
--   NULL
--   (as invented by Sir Tony Hoare)
-- replace with Maybe (in Haskell)
--         with Optional<T> (in Java)

-- "Parse, Don't Validate"
------------------------------------------------------------------------




------------------------------------------------------------------------
-- "Make Illegal States Unrepresentable"

-- DEFINE Metres, Seconds, MetresPerSecond

newtype Metres = MkMetres { getMetres :: Integer }
newtype Seconds = MkSeconds { getSeconds :: Integer }
newtype MetresPerSecond = MkMetresPerSecond { getMetresPerSecond :: Double }

distanceToTheMoon :: Metres
distanceToTheMoon = MkMetres 34987394875

secondsInAnHour :: Seconds
secondsInAnHour = MkSeconds (60*60)

computeSpeed :: Metres -> Seconds -> MetresPerSecond
computeSpeed (MkMetres d) (MkSeconds s)
  = MkMetresPerSecond (fromIntegral d / fromIntegral s)


-- F# programming language from Microsoft
--   units of measure types built in



------------------------------------------------------------------------
-- "Parse, don't validate"

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


newtype DSUsername = MkDSUsername String
newtype RegistrationNumber = MkRegistrationNumber String

-- Every student record has at least one of these,


-- DEFINE These

data These a b
  = This a
  | That b
  | These a b


-- DEFINE Student

data Student' k = MkStudent
  { name :: String
  , uniqueID :: k DSUsername RegistrationNumber
  }

type Student = Student' These
type SuperStudent = Student' (,)

mkSuper :: Student -> IO SuperStudent
mkSuper = undefined

-- GIVE an example of a student
gallais :: Student
gallais = MkStudent "Guillaume Allais" (This (MkDSUsername "clb11207"))


-- DISCUSS *abstract* datatypes and module interfaces

-- mkDSUSername :: String -> IO (Maybe DSUsername)

-- module StudentID (Student, DSUsername, getDSUsername, Registration)



------------------------------------------------------------------------
-- Type Classes I: Eq, Show, Algebraic structures


-- DEFINE MyShow

class MyShow a where
   myShow :: a -> String

-- DEFINE case insensitive strings & add Show / Eq instance


newtype CaseInsensitiveString = MKCIString String

instance MyShow CaseInsensitiveString where
   myShow (MKCIString str) = map toUpper str



-- Type class <~~~~~> interface in Java

-- public class JHGJH implements X, Y, Z

-- On Friday:
--  - Monoids -- generalising addition, multiplication, and, or, concatenation, ...
--  - Foldable, Functor


-- Semigroups
class Semigroup m where
  (<>) :: m -> m -> m
-- DISCUSS laws

-- associativity - (x <> y) <> z = x <> (y <> z)

instance Semigroup [a] where
  (<>) = (++)

newtype Tsil a = MkTsil { getTsil :: [a] } deriving (Show, Eq, Arbitrary)

instance Semigroup (Tsil a) where
  MkTsil xs <> MkTsil ys = MkTsil (ys ++ xs)

newtype Empty a = MkEmpty { getEmpty :: [a] } deriving (Show, Eq, Arbitrary)

instance Semigroup (Empty a) where
  _ <> _ = MkEmpty []

newtype Sum = MkSum {getSum :: Int} deriving (Show, Eq, Arbitrary)

instance Semigroup Sum where
  MkSum n <> MkSum m = MkSum (n + m)

newtype Prod = MkProd {getProd :: Int} deriving (Show, Eq, Arbitrary)

instance Semigroup Prod where
  MkProd n <> MkProd m = MkProd (n*m)

newtype Max = MkMax {getMax :: Int} deriving (Show, Eq, Arbitrary)

instance Semigroup Max where
  MkMax n <> MkMax m = MkMax (max n m)

{-
newtype Sub = MkSub { getSub :: Int } deriving (Show, Eq, Arbitrary)

instance Semigroup Sub where
  MkSub m <> MkSub n = MkSub (m - n)
-}

-- DEFINE various instances



test :: Semigroup m => (Int -> m) -> m
test f = f 0 <> f 1 <> f 2


-- DEFINE property tests

prop_associative
  :: (Eq m, Semigroup m)
  => (Int -> m)
  -> m -> m -> m -> Bool
prop_associative f x y z = x <> (y <> z) == (x <> y) <> z

-- DEFINE non-examples (e.g. minus)


-- Monoids
class Semigroup m => Monoid m where
  mempty :: m
-- DISCUSS laws
-- DEFINE property tests

prop_neutral
  :: (Eq m, Monoid m)
  => (Int -> m)
  -> m -> Bool
prop_neutral f x =
   x == mempty <> x
   && x <> mempty == x

-- EXTEND previous instances (if possible!)

instance Monoid [a] where
  mempty = []

instance Monoid (Tsil a) where
  mempty = MkTsil []

{-
instance Monoid (Empty a) where
  mempty = MkEmpty []
-}

instance Monoid Sum where
  mempty = MkSum 0

instance Monoid Prod where
  mempty = MkProd 1

instance Monoid Max where
  mempty = MkMax minBound -- sneaky!


------------------------------------------------------------------------
-- Type Classes II: Iterators


-- DEFINE foldList
foldList :: Monoid m => [m] -> m
foldList [] = mempty
foldList (m:ms) = m <> foldList ms


prop_foldList :: (Eq m, Monoid m)
  => (Int -> m)
  -> [m] -> [m] -> Bool
prop_foldList f xs ys = foldList (xs <> ys) == foldList xs <> foldList ys


newtype First a = MkFirst {getFirst :: Maybe a} deriving Show


-- DEFINE First Monoid
instance Semigroup (First a) where
  (MkFirst Nothing) <> x = x
  x <> _ = x

instance Monoid (First a) where
  mempty = MkFirst Nothing



-- newtype

-- EXAMPES



-- DEFINE Foldable

class Foldable t where
  fold :: Monoid m => t m -> m

data Formula a = Atom a | IsTrue | And (Formula a) (Formula a)


instance Foldable Formula where
   fold (Atom x) = x
   fold IsTrue   = mempty
   fold (And e f) = fold e <> fold f


allAtoms :: Formula a -> [a]
allAtoms = fold . fmap (\ a -> [a])
-- DEFINE various instances


-- DEFINE Functor
-- DEFINE various instances


-- EXAMPLES foldMap
