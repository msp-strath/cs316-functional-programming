{-# OPTIONS -fwarn-incomplete-patterns #-}
module Week05Live where

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
