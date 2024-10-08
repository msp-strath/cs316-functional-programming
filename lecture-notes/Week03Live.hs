{-# LANGUAGE ScopedTypeVariables #-}

module Week03Live where

import Prelude hiding (id, ($), (.), flip, map, filter)

-- Week 03 : HIGHER ORDER FUNCTIONS

-- This week : coursework to be released before Friday lecture.
--             worth 50%
-- Deadline  : 17:00 Tuesday 3rd December 2024

id :: forall a. a -> a
id x = x

($) :: forall a b. (a -> b) -> a -> b
($) = id

-- Composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

-- Pipe (|>)
(|>) :: forall a b. a -> (a -> b) -> b
(|>) = flip ($)
-- (|>) a f = f a

-- flip
flip :: forall a b c. (a -> b -> c) -> (b -> (a -> c))
flip f b a = f a b


-- partialApply
partialApply :: ((a, b) -> c) -> a -> (b -> c)
partialApply f x = \ y -> f (x, y)



------------------------------------------------------------------------------

-- map
map :: forall a b. (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

-- filter
filter :: (a -> Bool) -- test p
       -> [a]         -- values xs
       -> [a]         -- only the x in xs that satisfy p
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs


-- dupAll
dupAll :: [a] -> [a]
dupAll xs = xs |> map (\x -> [x,x]) |> concat
      --    xs |> map (\x -> x : x : )

-- Duplicating every element of a list by generating two-element lists
-- and then concatenating:

-- [1,2,3,4]
-- map (\x -> [x,x])    gives [[1,1], [2,2], [3,3], [4,4]]
-- concat               gives [1,1,2,2,3,3,4,4]

-- dupAll [1,2,3,4] = [1,1,2,2,3,3,4,4]


-- What if we do something else in the mop?
--
-- Instead of constructing a two-element list for every element of the
-- input list, what if we return a _function_ that prepends two
-- elements to a list?
--
--   [1,2,3,4] |> map (\x ys -> x : x : ys)
-- gives
--   [\ys -> 1 : 1 : ys, \ys -> 2 : 2 : ys, \ys -> 3 : 3 : ys, \ys -> 4 : 4 : ys]
--
-- If we now 'map (\f -> f [])' after this, we fill in each 'ys' with
-- '[]', then we get a list of two-element lists again:
--
--   [1,2,3,4] |> map (\x ys -> x : x : ys) |> map (\f -> f [])
-- gives
--   [[1,1],[2,2],[3,3],[4,4]]
--
-- But there are more things we can do. Since we have a list of
-- functions, we can compose them all together with a function like
-- this:

composeAll :: [a -> a] -> a -> a
composeAll []     = id
composeAll (f:fs) = f . composeAll fs

--   [1,2,3,4] |> map (\x ys -> x : x : ys) |> composeAll
-- gives
--   [1,1,2,2,3,3,4,4]
--
-- Exercise: Why? Can you write out the steps that lead to this
-- answer?
--
-- So we get the same answer as before, but this idea of taking the
-- rest of the output as 'ys' enables extra power. Effectively we are
-- getting access to the “future” result. In this example we are then
-- prepending two copies of each element to this future result. But we
-- can do a bit more:
--
--   [1,2,3,4] |> map (\x ys -> [x] ++ ys ++ [x]) |> composeAll
-- gives
--   [1,2,3,4,4,3,2,1]
--
-- At every step, this puts each element of the input at the beginning
-- and end of the rest of the results, leading to this "balanced"
-- output.
--
-- This kind of "access to the future" is surprisingly useful. In some
-- programming languages it is possible to queue up things to do after
-- the current task has finished. In the Go programming langauge, for
-- example, there is a 'defer' instruction which adds some code to run
-- when the current function finishes. This is used to add "clean up"
-- code, similar to 'finally' blocks in Java. See
-- https://go.dev/blog/defer-panic-and-recover .


------------------------------------------------------------------------------

-- We didn't do this in the lecture, but it is similar to the
-- exercises at the end of the Week 03 Problems file.

data Formula a
  = Atom a
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Not (Formula a)
  deriving Show

eval :: Formula Bool -> Bool
eval (Atom b)  = b
eval (And p q) = eval p && eval q
eval (Or p q)  = eval p || eval q
eval (Not p)   = not (eval p)

evalWith :: (a -> Bool) -> Formula a -> Bool
evalWith valuation (Atom a)  = valuation a
evalWith valuation (And p q) = evalWith valuation p && evalWith valuation q
evalWith valuation (Or p q)  = evalWith valuation q || evalWith valuation q
evalWith valuation (Not p)   = not (evalWith valuation p)

mapFormula :: (a -> b) -> Formula a -> Formula b
mapFormula f (Atom a)  = Atom (f a)
mapFormula f (And p q) = And (mapFormula f p) (mapFormula f q)
mapFormula f (Or p q)  = Or (mapFormula f p) (mapFormula f q)
mapFormula f (Not p)   = Not (mapFormula f p)
