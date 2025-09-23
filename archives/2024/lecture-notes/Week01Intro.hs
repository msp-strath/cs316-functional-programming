{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Week01Intro where

{-    WELCOME TO

        CS316

          FUNCTIONAL PROGRAMMING


      with
        Guillaume Allais
        Robert Atkey
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
     - Tuesdays at 11:00
     - Fridays  at 11:00

     Tuesdays at 12:00-16:00 : Labs in Level 12 of Livingstone Tower

   - Holes:
     - No lecture on Tuesday 1st October

   - Video lectures, to support the in-person lectures
     - ~ 6 videos / week
     - ~ 10 minutes long

   - Online lecture notes in a GitHub repository
     - git clone https://github.com/msp-strath/cs316-functional-programming
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
        Specification released Week 3
        Submission Week 11


 Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }

-}


-- Playing cards
data Suit = Diamonds | Hearts | Clubs | Spades | Circle
  deriving (Show)

exampleSuit :: Suit
exampleSuit = Diamonds


data Colour = Red | Black
  deriving (Show)

colourOfSuit :: Suit -> Colour
colourOfSuit Diamonds = Red
colourOfSuit Hearts = Red
colourOfSuit Spades = Black
colourOfSuit Circle = Red
colourOfSuit Clubs = Black

data Value
  = Ace
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | N10
  | Jack
  | Queen
  | King
  deriving (Show)

numericValue :: Value -> Int
numericValue = \ x -> case x of
  Ace -> 1
  N2 -> 2
  N3 -> 3
  N4 -> 4
  N5 -> 5
  N6 -> 6
  N7 -> 7
  N8 -> 8
  N9 -> 9
  N10 -> 10
  Jack -> 11
  Queen -> 12
  King -> 13

lessThanOrEqualValue :: Value -> Value -> Bool
lessThanOrEqualValue v1 v2 =
  numericValue v1 <= numericValue v2


data Card = MkCard Suit Value
  deriving (Show)

suitOfCard :: Card -> Suit
suitOfCard (MkCard suit _) = suit



{-
   suitOfCard (MkCard Hearts Queen)



-}
