{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
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

-- DEFINE various instances



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
