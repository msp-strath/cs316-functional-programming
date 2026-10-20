{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving #-}
module Week05Live where

import Prelude hiding (Semigroup (..), Monoid (..), Foldable (..), Functor (..))
import Data.Char

import Test.QuickCheck

------------------------------------------------------------------------
-- Type Classes I: Eq, Show, Algebraic structures


-- DEFINE MyShow

class MyShow a where
   myShow :: a -> String

-- DEFINE case insensitive strings & add Show / Eq instance






-- Type class <~~~~~> interface in Java

-- public class JHGJH implements X, Y, Z


-- Semigroups
class Semigroup m where
  (<>) :: m -> m -> m
-- DISCUSS laws
-- associativity - (x <> y) <> z = x <> (y <> z)

-- DEFINE various instances
newtype Int1 = MkInt1 { getInt1 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int1 where
  MkInt1 x <> MkInt1 y = MkInt1 (x + y)
instance Monoid Int1 where
  mempty = MkInt1 0

newtype Int2 = MkInt2 { getInt2 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int2 where
  MkInt2 x <> MkInt2 y = MkInt2 (x * y)
instance Monoid Int2 where
  mempty = MkInt2 1

newtype Int4 = MkInt4 { getInt4 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int4 where
  MkInt4 x <> MkInt4 y = MkInt4 0
-- Not a monoid! We throw away all information!

newtype Int5 = MkInt5 { getInt5 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int5 where
  MkInt5 x <> MkInt5 y = MkInt5 (max x y)
instance Monoid Int5 where
  mempty = MkInt5 minBound

newtype Int6 = MkInt6 { getInt6 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int6 where
  MkInt6 x <> MkInt6 y = MkInt6 (min x y)
instance Monoid Int6 where
  mempty = MkInt6 maxBound

newtype Int7 = MkInt7 { getInt7 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int7 where
  MkInt7 x <> MkInt7 y = MkInt7 x
-- Not a monoid!

newtype Int8 = MkInt8 { getInt8 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int8 where
  MkInt8 x <> MkInt8 y = MkInt8 y
-- Not a monoid!

-- counter-examples

newtype Int3 = MkInt3 { getInt3 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int3 where
  MkInt3 x <> MkInt3 y = MkInt3 (read (show x ++ show y))

newtype Int9 = MkInt9 { getInt9 :: Int }
  deriving (Show, Eq, Arbitrary)
instance Semigroup Int9 where
  MkInt9 x <> MkInt9 y = MkInt9 (x - y)



test :: Semigroup m => (Int -> m) -> m
test f = f 0 <> f 1 <> f 2


-- DEFINE property tests

prop_associative
  :: (Eq m, Semigroup m)
  => (Int -> m)
  -> m -> m -> m -> Bool
prop_associative f x y z = x <> (y <> z) == (x <> y) <> z

-- DEFINE non-examples





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


-- A small question:
-- Given  m has a (Semigroup m) structure
-- Can you define m' that has a (Monoid m') structure?

newtype First a = MkFirst { getFirst :: Maybe a }

instance Semigroup (First a) where
  MkFirst Nothing <> y = y
  x <> _ = x

instace Monoid (First a) where
  mempty = MkFirst Nothing










------------------------------------------------------------------------
-- Type Classes II: Iterators


-- DEFINE foldList
foldList :: Monoid m => [m] -> m
foldList = undefined

{-
prop_foldList :: (Eq m, Monoid m)
  => (Int -> m)
  -> [m] -> [m] -> Bool
prop_foldList f xs ys = foldList (xs <> ys) == foldList xs <> foldList ys
-}

-- DEFINE First Monoid



-- newtype

-- EXAMPLES



-- DEFINE Foldable

class Foldable t where
  fold :: Monoid m => t m -> m

-- Formula

-- DEFINE various instances

-- DEFINE Functor

class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

  -- laws:
prop_IdentityLaw :: (Eq (f a), Functor f) => (Int -> f Int) -> f a -> Bool
prop_IdentityLaw _ t = fmap id t == t

prop_CompositionLaw
  :: (Eq (f c), Functor f)
  => (Int -> f Int)
  -> (a -> b) -> (b -> c) -> f a
  -> Bool
prop_CompositionLaw _ first second t =
  fmap second (fmap first t) == fmap (second . first) t

-- DEFINE various instances

-- EXAMPLES foldMap
