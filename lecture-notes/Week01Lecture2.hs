{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Lecture2 where

import Data.List

-- Talk: In the Engine Room of LLMs
-- By: Satnam Singh (Groq Inc)
-- Where: RC513 Wednesday 11am 2nd October


data Markup
  = Text String
  | Bold Markup
  | Italic Markup
  | Concat Markup Markup
  deriving (Show, Eq)

-- Pandoc tool

-- Example?
example :: Markup
example = Concat (Text "Hello") (Concat (Text " ") (Bold (Text "world")))

-- Hello **world**

-- Hello <strong>world</strong>





-- catMarkup
catMarkup :: [Markup] -> Markup
catMarkup [] = Text ""
catMarkup [x] = x
catMarkup (x : xs) = Concat x (catMarkup xs)


-- catMarkupSpaced [Text "hello", Text "world"]
--   Concat (Text "hello") (Concat (Text " ") (Text "world"))

catMarkupSpaced :: [Markup] -> Markup
catMarkupSpaced [] = Text ""
catMarkupSpaced [x] = x
catMarkupSpaced (x : xs) = Concat x (Concat (Text " ") (catMarkupSpaced xs))

catMarkupSpaced_v2 :: [Markup] -> Markup
catMarkupSpaced_v2 xs = catMarkup (intersperse (Text " ") xs)
                -- = catMarkup . intersperse (Text " ")

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (g . f) x = g (f x)

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(f |> g) x = g (f x)

sepBy :: Markup -> [Markup] -> Markup
sepBy separator = intersperse separator |> catMarkup

list :: [Markup] -> Markup
list xs = Concat (Text "[") (Concat (sepBy (Text ", ") xs) (Text "]"))

between :: Markup -> Markup -> (Markup -> Markup)
between l r xs = Concat l (Concat xs r)

bracket = between (Text "[") (Text "]")

strings :: [Markup] -> Markup
strings = map (between (Text "\"") (Text "\"")) |> list


-- markupToHTML

-- markupToHTML :: Markup -> String
--    "Bob" --;
--     DROP TABLE users;

-- "Little Bobby Tables"

strong :: [HTML] -> HTML
strong htmls = HEl "strong" htmls

em htmls = HEl "em" htmls

data HTML = HText String
          | HEl String [HTML]
      --    | HConcat HTML HTML
  deriving (Show, Eq)

markupToHTML :: Markup -> [HTML]
markupToHTML (Text s) = [HText s]
markupToHTML (Bold m) = [strong (markupToHTML m)]
markupToHTML (Italic m) = [em (markupToHTML m)]
markupToHTML (Concat m1 m2) =
    markupToHTML m1 ++ markupToHTML m2



-- escapeHTML
