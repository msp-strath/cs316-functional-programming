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
data Tree a = Leaf | Node (Tree a) a (Tree a)

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


-- 4. Type classes

-- Eq, Show, Ord



--- Next time: Semigroups, Monoids, Foldable, Functor
