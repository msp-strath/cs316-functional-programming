{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Week01Live where

import Data.List (intersperse)

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
     - Tuesdays at 11am
     - Fridays  at 2pm

   - Labs from Week 3 in Level 12 of Livingstone Tower
     - Tuesdays at 13:00-15:00 :

   - Holes:
     - No lecture on Tuesday 29th September

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
        First attempt:    Week 6 (October 27th)

   - One large coursework "mini-project" (50%)
        Specification released Week 3
        Submission Week 11 (December 11th)


 Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }

-}


-- Playing cards

------------------------------------------------------------------------------
-- Suits, and their colours

-- exampleSuit :: Suit
data Suit = Diamonds | Clubs | Hearts | Spades
  deriving(Show, Eq, Enum, Bounded)

exampleSuit :: Suit
exampleSuit = Clubs

data Colour = Red | Black
  deriving Show

getColour :: Suit -> Colour
getColour Diamonds = Red
getColour Clubs    = Black
getColour Hearts   = Red
getColour Spades   = Black

------------------------------------------------------------------------------
-- Modeling ranks
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King
       deriving(Show, Enum, Eq, Ord, Bounded)

numericValue :: Rank -> Int
numericValue x = case x of
  Ace   -> 11
  Two   -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 10
  Queen -> 10
  King -> 10

lessThanOrEqualValue :: Rank -> Rank -> Bool
lessThanOrEqualValue r1 r2 = numericValue r1 <= numericValue r2

------------------------------------------------------------------------------
-- A card is a suit together with a rank

data Card = MkCard
  { getSuit :: Suit
  , getRank :: Rank
  }
 deriving(Show, Eq)

suitOfCard :: Card -> Suit
suitOfCard (MkCard s r) = s

suitOfCard' :: Card -> Suit
suitOfCard' = getSuit

changeSuitOfCard :: Suit -> Card -> Card
changeSuitOfCard s (MkCard _ r) = MkCard s r

changeSuitOfCard' :: Suit -> Card -> Card
changeSuitOfCard' s c = c { getSuit = s }

------------------------------------------------------------------------------
-- Generating a deck

allSuits :: [Suit]
allSuits = [minBound..maxBound]

allRanks :: [Rank]
allRanks = [minBound..maxBound]

-- Using a list comprehension
allCards :: [Card]
allCards = [MkCard suit rank | suit <- allSuits, rank <- allRanks]

-- Using applicative notations
deck :: [Card]
deck = pure MkCard <*> allSuits <*> allRanks
{-
  do suit <- allSuits
     rank <- allRanks
     pure (MkCard suit rank)
-}










------------------------------------------------------------------------------
-- Beyond enumerations and lists

-- A small Markup data structure
-- Related work: This is the type of generic representation used in e.g. pandoc
-- https://pandoc.org/


-- DEFINE data Markup
-- text, bold, italic, concat
data Markup
  = Text String
  | Bold Markup
  | Italic Markup
  | Concat Markup Markup
  deriving (Show)

smartConcat :: Markup -> Markup -> Markup
smartConcat (Text str1) (Text str2) = Text (str1 ++ str2)
smartConcat (Text "") doc = doc
smartConcat doc (Text "") = doc
-- Pro move
-- smartConcat (Concat l m) r = smartConcat l (smartConcat m r)
smartConcat l r = Concat l r

-- DEFINE an example: hello world (with some random formatting)

exampleMarkup :: Markup
exampleMarkup =
  smartConcat (smartConcat (Text "Hell") (Text "o"))
         (smartConcat (Italic (Text " ")) (Bold (Text "World!")))

-- DISCUSS syntax vs. semantics based on example
-- REFACTOR (?)

-- Markdown: Hello **World!**
-- HTML:     Hello <strong>World!</strong>
-- Forgetful:    Hello World!

forgetful :: Markup -> String
forgetful (Text str) = str
forgetful (Bold doc) = forgetful doc
forgetful (Italic doc) = forgetful doc
forgetful (Concat doc1 doc2) = forgetful doc1 ++ forgetful doc2

------------------------------------------------------------------------------
-- DISCUSS (Domain Specific) Languages
-- Nouns: datatypes / ground values
-- Verbs: functions


-- DEFINE catMarkup
catMarkup :: [Markup] -> Markup
catMarkup [] = Text ""
catMarkup [doc] = doc
catMarkup (doc : docs) = Concat doc (catMarkup docs)

-- DEFINE
-- catMarkupSpaced [Text "hello", Text "world"]
--   Concat (Text "hello") (Concat (Text " ") (Text "world"))

catMarkupSpaced :: [Markup] -> Markup
catMarkupSpaced []     = Text ""
catMarkupSpaced [x]    = x
catMarkupSpaced (x:xs) = catMarkup [x,(Text " "),catMarkupSpaced xs]

catMarkupSpaced' :: [Markup] -> Markup
catMarkupSpaced' docs = catMarkup (intersperse (Text " ") docs)

-- REFACTOR using intersperse
-- REFACTOR as punctuate

punctuate :: Markup -> [Markup] -> Markup
punctuate sep docs = catMarkup (intersperse sep docs)


-- DEFINE list :: [Markup] -> Markup
-- DEFINE set  :: [Markup] -> Markup

-- REFACTOR list, set using between

------------------------------------------------------------------------------
-- A small Markup semantics

-- DEFINE HTML

-- data HTML

-- type Domain = [HTML]


-- DEFINE markupToHTML
-- markupToHTML :: Markup -> Domain

-- DISCUSS (and DEFINE?) escapeHTML

-- DISCUSS generalising Markup?
