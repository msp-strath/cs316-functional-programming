{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week05Live where

import Prelude hiding (Semigroup (..), Monoid (..), Foldable (..), Functor (..))
import Data.Char

-- Next week: 28th October: Class Test (1-3pm LT1201, 50%).

------------------------------------------------------------------------
-- TYPES TYPES TYPES

-- “Make Illegal States Unrepresentable”

-- “Billion Dollar Mistake”
--   NULL
--   (as invented by Sir Tony Hoare)
-- replace with Maybe (in Haskell)
--         with Optional<T> (in Java)

-- "Parse, Don't Validate"
------------------------------------------------------------------------




------------------------------------------------------------------------
-- "Make Illegal States Unrepresentable"

-- DEFINE Meters, Seconds, MetresPerSecond

-- distanceToTheMoon :: Metres
-- distanceToTheMoon = MkMetres 34987394875

-- DEFINE secondsInAnHour :: Seconds


-- DEFINE computeSpeed


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




------------------------------------------------------------------------
-- "Parse, don't validate"


newtype DSUsername = MkDSUsername String
newtype RegistrationNumber = MkRegistrationNumber String

-- Every student record has at least one of these,


-- DEFINE These

-- DEFINE Student

data Student = MkStudent
  { name    :: String
  , regInfo :: These DSUsername RegistrationNumber
  }

-- GIVE an example of a student


-- DEFINE mkRegistrationNumber :: String -> Maybe RegistrationNumber


-- DISCUSS *abstract* datatypes and module interfaces




------------------------------------------------------------------------
-- Type Classes I: Eq, Show, Algebraic structures


-- DEFINE MyShow

-- DEFINE case insensitive strings & add Show / Eq instance




-- Type class <~~~~~> interface in Java

-- public class JHGJH implements X, Y, Z

-- On Friday:
--  - Monoids -- generalising addition, multiplication, and, or, concatenation, ...
--  - Foldable, Functor


-- Semigroups
class Semigroup m where
  (<>) :: m -> m -> m
-- DISCUSS laws

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
