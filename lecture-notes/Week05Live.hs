{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week05Live where

import Prelude hiding (Semigroup (..), Monoid (..), Foldable (..), Functor (..))
import Data.Char

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

newtype Tsil a = MkTsil { getTsil :: [a] } deriving Show

instance Semigroup (Tsil a) where
  MkTsil xs <> MkTsil ys = ys ++ xs

newtype Empty a = MkEmpty { getEmpty :: [a] } deriving Show

instance Semigroup (Empty a) where
  _ <> _ = MkEmpty []

newtype Sum = MkSum {getSum :: Int} deriving Show

instance Semigroup Sum where
  MkSum n <> MkSum m = MkSum (n + m)

newtype Prod = MkProd {getProd :: Int} deriving Show

instance Semigroup Prod where
  MkProd n <> MkProd m = MkProd (n*m)

newtype Max = MkMax {getMax :: Int} deriving Show

instance Semigroup Max where
  MkMax n <> MkMax m = MkMax (max n m)


-- DEFINE various instances



test :: Semigroup m => (Int -> m) -> m
test f = f 0 <> f 1 <> f 2




-- Monoids
class Semigroup m => Monoid m where
  mempty :: m
-- DISCUSS laws


-- EXTEND previous instances (if possible!)




------------------------------------------------------------------------
-- Type Classes II: Iterators


-- DEFINE foldList
-- DEFINE First Monoid
-- EXAMPES

-- DEFINE Foldable
-- DEFINE various instances


-- DEFINE Functor
-- DEFINE various instances


-- EXAMPLES foldMap
