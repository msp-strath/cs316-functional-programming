{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Lecture2 where

import Data.List (intersperse)


------------------------------------------------------------------------------
-- A small Markup data structure
-- Related work: This is the type of generic representation used in e.g. pandoc
-- https://pandoc.org/


-- DEFINE data Markup
-- text, bold, italic, concat
data Annotation = Bold | Italic
  deriving (Show)

data Markup
  = Text String
  | Ann Annotation Markup
  | Concat Markup Markup
  deriving (Show)

smartConcat :: Markup -> Markup -> Markup
smartConcat (Text s) (Text t) = Text (s ++ t)
smartConcat m n = Concat m n

-- DEFINE an example: hello world (with some random formatting)

exampleMarkup :: Markup
exampleMarkup
  = Concat (Text "Hello")
  $ Concat (Text " ")
  $ Ann Bold (Text "World")

-- DISCUSS syntax vs. semantics based on example

-- Markdown: Hello **World**
-- HTML:     Hello <strong>World</strong>


------------------------------------------------------------------------------
-- A small Markup language

-- DISCUSS Domain Specific Languages
-- Nouns: datatypes / ground values
-- Verbs: functions


-- DEFINE catMarkup
catMarkup :: [Markup] -> Markup
catMarkup []    = Text ""
catMarkup [x]   = x
catMarkup (x:xs)= smartConcat x (catMarkup xs)

test = [Text "Hello", Text "World"]


-- DEFINE
-- catMarkupSpaced [Text "hello", Text "world"]
--   Concat (Text "hello") (Concat (Text " ") (Text "world"))

catMarkupSpaced :: [Markup] -> Markup
catMarkupSpaced []   = Text ""
catMarkupSpaced [x]  = x
catMarkupSpaced (x:xs)
  = smartConcat x
  $ smartConcat (Text " ")
  $ catMarkupSpaced xs


-- catMarkupSpaced [a,b,c] == catMarkup [a, Text " ", b, Text " ", c]
catMarkupSpaced' :: [Markup] -> Markup
catMarkupSpaced' mks = catMarkup (intersperse (Text " ") mks)

-- REFACTOR as punctuate
punctuate :: Markup -> [Markup] -> Markup
punctuate sep mks = catMarkup (intersperse sep mks)


-- DEFINE list :: [Markup] -> Markup
-- DEFINE set  :: [Markup] -> Markup

-- [x,y,z]
list :: [Markup] -> Markup
list xs = catMarkup [Text "[", punctuate (Text ",") xs, Text "]"]

list' :: [Markup] -> Markup
list' = between (Text "[") (Text "]") . punctuate (Text ",")

-- {x,y,z}
set :: [Markup] -> Markup
set xs = catMarkup [Text "{", punctuate (Text ",") xs, Text "}"]

set' :: [Markup] -> Markup
set' = between (Text "[") (Text "]") . punctuate (Text ",")

-- REFACTOR list, set

between :: Markup -> Markup -> (Markup -> Markup)
between left right = \ middle -> catMarkup [left, middle, right]

string :: Markup -> Markup
string m = between (Text "\"") (Text "\"") m

------------------------------------------------------------------------------
-- A small Markup semantics

-- DEFINE HTML

data HTML
  = Simple String
  | Tag String [HTML]
  deriving (Show)

type Domain = [HTML]

ann :: Annotation -> (Domain -> Domain)
ann Bold   xs = [Tag "strong" xs]
ann Italic xs = [Tag "i" xs]

combine :: Domain -> Domain -> Domain
combine h i = h ++ i

markupToHTML :: Markup -> Domain
markupToHTML (Text str) = [Simple str]
markupToHTML (Ann a m) = ann a $ markupToHTML m
markupToHTML (Concat m n) = combine (markupToHTML m) (markupToHTML n)

-- DEFINE bold
-- DEFINE italic




-- DISCUSS Yet Another DSL!


-- DEFINE markupToHTML :: Markup -> [HTML]




-- DISCUSS (and DEFINE?) escapeHTML



-- DISCUSS generalising Markup?
