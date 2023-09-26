{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Intro where

{-    WELCOME TO

        CS316

          FUNCTIONAL PROGRAMMING
-}

























{- In this course, you will:

     - Learn more about Functional Programming (in Haskell)



   (Typed) Functional Programming is

     - Defining Datatypes To Represent Problems

     - Defining Functions To Create New Data From Old

   a.k.a "Value-oriented" programming.

   A "Functional Programming Language" is a programming language that
   is designed to make it easy to use Functional Programming ideas. -}













{- We use Haskell as an example Functional Programming Language.

     - Many languages now include ideas originally from Functional Programming.

        - Functions as values (a.k.a "lambdas")

        - "Algebraic" data types; "Make Illegal States Unrepresentable"

        - Immutability

        - Expressive Types

        - Errors as data, instead of Exceptions

        - No 'null' (the "Billion dollar mistake")

        - Close tracking of possible "side effects"

   Haskell is not perfect (I will grumble about it during the course
   [*]), but it does offer a place to learn about Functional
   Programming concepts without too many distractions.

   [*] "There are only two kinds of languages: the ones people
       complain about and the ones nobody uses.”  ― Bjarne Stroustrup,
       The C++ Programming Language
-}







{- Course arrangements:

   - Lectures:
     - Tuesdays at 11:00 : Lecture in SW105
     - Fridays  at 11:00 : Lecture in JA314

     Tuesdays at 12:00-16:00 : Labs in Level 12 of Livingstone Tower

   - Holes:
     - No lecture on Tuesday 26th

   - Video lectures, to support the in-person lectures
     - ~ 6 videos / week
     - ~ 10 minutes long

   - Online lecture notes in a GitHub repository
     - git clone https://github.com/bobatkey/CS316-2023.git
     - git pull

-}


{- This is a programming course

   You will be expected to do a lot of programming in order to understand
   the concepts.

   20 credit course : 12 hrs/week, 1 hour of videos, 2 of lectures, 2 labs.
-}















{- YOU WILL NEED A WORKING HASKELL INSTALLATION

   - Suggested setup:

       - GHCup (GHC, Cabal, HLS) + VSCode + Haskell extension.

       - I use Emacs in the videos and lectures.

   - There are instructions on MyPlace

   - I (unfortunately) cannot test on Windows, so I will need the
     class's help to iron out Windows problems.

-}









{- Assessment:

   - One class test (24 hrs) (50%)
        Week 6

   - Redemption test
        Week 9
        A second chance to do the test

   - One large coursework "mini-project" (50%)
        Specification released Week 3 (Monday 2nd October)
        Submission Week 11 (~ Monday 27th November)

-}






-- Playing cards; suits
data Suit = Diamonds | Hearts | Clubs | Spades
  deriving (Show, Eq)

data Colour = Red | Black
  deriving (Show)

colourOfSuit :: Suit -> Colour
colourOfSuit Diamonds = Red
colourOfSuit Hearts = Red
colourOfSuit Spades = Black
colourOfSuit Clubs = Black

data Value = Ace | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King
  deriving Show

numericOfValue :: Value -> Int
numericOfValue Ace = 1
numericOfValue N2 = 2
numericOfValue N3 = 3
numericOfValue N4 = 4
numericOfValue N5 = 5
numericOfValue N6 = 6
numericOfValue N7 = 7
numericOfValue N8 = 8
numericOfValue N9 = 9
numericOfValue N10 = 10
numericOfValue Jack = 11
numericOfValue Queen = 12
numericOfValue King = 13

compareValue :: Value -> (Value -> Bool)
compareValue v1 v2 = numericOfValue v1 <= numericOfValue v2

data Card = MkCard Suit Value
       --   MkCard :: Suit -> Value -> Card
  deriving Show

suitOfCard :: Card -> Suit
suitOfCard (MkCard suit _) = suit

-- suitOfCard (MkCard Spades N7)
--   -- suit = Spades
--   == Spades

valueOfCard :: Card -> Value
valueOfCard (MkCard _ value) = value


myHand = [ MkCard Hearts Ace
         , MkCard Diamonds N8
         , MkCard Spades Queen
         , MkCard Spades N2
         , MkCard Clubs N9
         ]



-- Sorting and filtering lists of cards
{-
ghci> map (\card -> suitOfCard card) myHand
[Hearts,Diamonds,Spades,Spades,Clubs]
ghci> nub (map (\card -> suitOfCard card) myHand)

<interactive>:16:1: error:
    Variable not in scope: nub :: [Suit] -> t
ghci> import Data.List
ghci> :r
[1 of 1] Compiling Week01Intro      ( Week01Intro.hs, interpreted )
Ok, one module loaded.
ghci> nub (map (\card -> suitOfCard card) myHand)
[Hearts,Diamonds,Spades,Clubs]
ghci> nub (map (\card -> suitOfCard card) (filter (\card -> compareValue (valueOfCard card) Queen) myHand)

<interactive>:20:101: error:
    parse error (possibly incorrect indentation or mismatched brackets)
ghci> nub (map (\card -> suitOfCard card) (filter (\card -> compareValue (valueOfCard card) Queen) myHand))
[Hearts,Diamonds,Spades,Clubs]
ghci> nub (map (\card -> suitOfCard card) (filter (\card -> compareValue (valueOfCard card) N4) myHand))
[Hearts,Spades]
ghci>
-}
