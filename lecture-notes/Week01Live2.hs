{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Lecture2 where

import Data.List


------------------------------------------------------------------------------
-- A small Markup data structure
-- Related work: This is the type of generic representation used in e.g. pandoc
-- https://pandoc.org/


-- DEFINE data Markup
-- text, bold, italic, concat


-- DEFINE an example: hello world (with some random formatting)


-- DISCUSS syntax vs. semantics based on example



------------------------------------------------------------------------------
-- A small Markup language

-- DISCUSS Domain Specific Languages
-- Nouns: datatypes / ground values
-- Verbs: functions


-- DEFINE catMarkup
-- catMarkup :: [Markup] -> Markup


-- DEFINE
-- catMarkupSpaced [Text "hello", Text "world"]
--   Concat (Text "hello") (Concat (Text " ") (Text "world"))

-- catMarkupSpaced :: [Markup] -> Markup

-- REFACTOR as punctuate


-- DEFINE list :: [Markup] -> Markup
-- DEFINE set  :: [Markup] -> Markup

-- REFACTOR list, set


------------------------------------------------------------------------------
-- A small Markup semantics

-- DEFINE HTML


-- DEFINE bold
-- DEFINE italic


-- DISCUSS Yet Another DSL!


-- DEFINE markupToHTML :: Markup -> [HTML]




-- DISCUSS (and DEFINE?) escapeHTML



-- DISCUSS generalising Markup?
