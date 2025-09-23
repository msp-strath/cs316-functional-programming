module Week10Live2 where

import Test.QuickCheck

{- QuickCheck is a library for Property Based Testing

   Instead of individual tests:
     - “On input X, expect to get output Y”

   You define properties, usually of the form:
     - for all x, y, z. P(x,y,z)
   where P is something executable.

   QuickCheck then generates random values for 'x', 'y', and 'z' and
   tries to find a counterexample: some values that make P(x,y,z)
   false.

   Logically, we are trying to prove a universal statement:

       for all x, y, z. P(x,y,z)

   QuickCheck operates by trying to prove the negation of this
   statement, which is an existential statement:

       exists x, y, z. not P(x,y,z)

   There are two possible outcomes:

     1. QuickCheck finds an 'x', 'y', 'z' that makes P(x,y,z) false,
        which is a proof that the original statement is false.

     2. QuickCheck fails to find an 'x', 'y', 'z' that make P(x,y,z)
        false, which could mean either:
         (a) the original statement is true (cool!)
         (b) the original statement is not true, but we just
             haven't found a counterexample (less cool)

   These two outcomes are true of any kind of testing (except in
   special cases where a finite number of tests suffices). In general,
   the slogan is "Testing can only prove the presence of errors, not
   their absence." (something like this was said by Edsger Dijkstra).

   Nevertheless, testing is extremely useful and nigh essential for
   making reliable software. Property-Based Testing as in QuickCheck
   is a useful tool in addition to normal testing because:

     1. It encourages you to think about and write down general
        /properties/ instead of individual test cases. These
        properties can often be useful for other developers trying to
        understand your code.

     2. Often, there are well-known properties that apply in many
        situations. Trying to write down complex properties is often a
        sign that your code is too complex and contains too many
        special cases.

     3. The /randomised/ aspect of QuickCheck is good at generating
        examples that you may not have thought of, so the overall test
        coverage can be improved. (CAUTION: it might also be the case
        that the random generator is biased and just fails to generate
        some potential counterexamples)

   QuickCheck is still largely a "blackbox" testing tool, in that it
   doesn't look at the property to try to generate specific
   counterexamples, it just obliviously generates them and runs the
   test. Some tools (e.g. fuzzers) do a more directed search for
   counterexamples. -}


{- Example : MONOIDS -}

{- The monoid laws:

   1. for all x. mempty <> x == x
   2. for all x. x <> mempty == x
   3. for all x y z. (x <> y) <> z == x <> (y <> z)

-}

monoid_left_unit_law :: (Eq a, Monoid a) => a -> Bool
monoid_left_unit_law x = mempty <> x == x

monoid_right_unit_law :: (Eq a, Monoid a) => a -> Bool
monoid_right_unit_law x = x <> mempty == x

monoid_assoc_law :: (Eq a, Semigroup a) => a -> a -> a -> Bool
monoid_assoc_law x y z = (x <> y) <> z == x <> (y <> z)

monoid_laws :: (Show a, Eq a, Monoid a) => Gen a -> Property
monoid_laws gen =
  conjoin [ forAll gen $ \x -> monoid_left_unit_law x
          , forAll gen $ \x -> monoid_right_unit_law x
          , forAll gen $ \x ->
              forAll gen $ \y ->
                forAll gen $ \z ->
                  monoid_assoc_law x y z
          ]

-- Good examples:

newtype And = MkAnd Bool deriving (Show, Eq)
instance Semigroup And where
  MkAnd x <> MkAnd y = MkAnd (x && y)
instance Monoid And where
  mempty = MkAnd True
instance Arbitrary And where
  arbitrary = MkAnd <$> arbitrary
         -- arbitrary :: Gen Bool


-- Bad examples:

instance Semigroup Double where
  x <> y = x + y

-- Rock, Paper, Scissors
data RPS = Rock | Paper | Scissors deriving (Eq, Show)

instance Arbitrary RPS where
  arbitrary = oneof [ pure Rock, pure Paper, pure Scissors ]

instance Semigroup RPS where
  Rock     <> Rock     = Rock
  Rock     <> Scissors = Rock
  Rock     <> Paper    = Paper
  Paper    <> Paper    = Paper
  Paper    <> Rock     = Paper
  Paper    <> Scissors = Scissors
  Scissors <> Scissors = Scissors
  Scissors <> Paper    = Paper
  Scissors <> Rock     = Rock



{- Example : GENERATORS -}

data JSON
  = Number  Int
  | Boolean Bool
  | String  String
  | Null
  | Array   [JSON]
  | Object  [(String,JSON)]
  deriving (Show, Eq)

-- Example from: https://typeable.io/blog/2021-08-09-pbt.html
instance Arbitrary JSON where
  arbitrary = sized arbitrary'
    where
      arbitrary' 0 = pure $ Array []
      arbitrary' n =
        oneof [ Object <$> resize (n `div` 2) arbitrary
              , Array <$> resize (n `div` 2) arbitrary
              , String <$> arbitrary
              , Number <$> arbitrary
              , Boolean <$> arbitrary
              , pure Null
              ]
