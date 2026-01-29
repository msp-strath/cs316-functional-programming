{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Week01Intro where

{-    WELCOME TO

        CS316 λ>=

          FUNCTIONAL PROGRAMMING


      with
        Alasdair Lambert
        Guillaume Allais

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

   - Labs in Level 12 of Livingstone Tower
     - Tuesdays at 13:00-15:00 :

   - Holes:
     - No lecture on Tuesday 30th September

   - Video lectures, to support the in-person lectures
     - ~ 6 videos / week
     - ~ 10 minutes long

   - Online lecture notes in a GitHub repository
     - git clone https://github.com/msp-strath/cs316-functional-programming
     - git pull

     Feel free to send PRs if you spot mistakes!

-}


{- This is a programming course

   You will be expected to do a lot of programming in order to understand
   the concepts.

   20 credit course : 12 hrs/week, 1 hour of videos, 2 of lectures, 2 labs.
-}















{- YOU WILL NEED A WORKING HASKELL INSTALLATION

   - Suggested setup:

       - GHCup (GHC, Cabal, HLS) + VSCode + Haskell extension.

       - We use Emacs in the lectures and so does Bob in the videos

   - There are instructions on MyPlace

   - We (unfortunately) cannot test on Windows, so we will need the
     class's help to iron out Windows problems.

-}









{- Assessment:

   - One class test (50%) in the labs mediated via myplace
        First attempt:    Week 6 (October 28th)
        Redemption test:  Week 9 (November 18th)
     We will only keep the best of both marks

   - One large coursework "mini-project" (50%)
        Specification released Week 3
        Submission Week 11 (December 11th)


 Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }

-}


-- Playing cards

------------------------------------------------------------------------------
-- Suits, and their colours

data Suit = Diamonds | Hearts | Spades | Clubs
  deriving (Show,Enum,Bounded)

exampleSuit :: Suit
exampleSuit = Hearts

data Colour = Red | Black
  deriving (Show)
-- Why not Bool? Because `Red` and `Black` have clear
-- domain-specific meaning that `True` and `False` simply
-- do not have!

getColour :: Suit -> Colour
getColour Hearts = Red
getColour Spades = Black
getColour Clubs = Black
getColour Diamonds = Red


------------------------------------------------------------------------------
-- Modeling ranks (compare to last year's definition!)

data Rank where
  Number :: Int -> Rank
  Jack   :: Rank
  Queen  :: Rank
  King   :: Rank
  deriving (Show)

problematicRank :: Rank
problematicRank = Number (-42)

numericValue :: Rank -> Int
numericValue = \case
  Number n -> n
  Jack -> 11
  Queen -> 12
  King -> 13

lessThanOrEqualValue :: Rank -> Rank -> Bool
lessThanOrEqualValue v1 v2 = numericValue v1 <= numericValue v2


------------------------------------------------------------------------------
-- A card is a suit together with a rank

data Card = MkCard
  { getSuit :: Suit
  , getRank :: Rank
  } deriving (Show)

suitOfCard :: Card -> Suit
suitOfCard = getSuit


suitOfCard' :: Card -> Suit
suitOfCard' (MkCard s r) = s

-- frugality principle

------------------------------------------------------------------------------
-- Generating a deck

allSuits :: [Suit]
allSuits = [minBound .. maxBound]

allRanks :: [Rank]
allRanks
  = (Number <$> [1..10]) ++ [Jack,Queen,King]

-- Using a list comprehension
deck :: [Card]
deck = [ MkCard x y | x <- allSuits, y <- allRanks]

-- Using applicative notations
deck' :: [Card]
deck' = MkCard <$> allSuits <*> allRanks
