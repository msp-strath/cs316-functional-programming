{-# LANGUAGE ScopedTypeVariables #-}

module Week03Live where

import Prelude hiding (id, ($), (.), flip, map, filter)

-- Week 03 : HIGHER ORDER FUNCTIONS



id :: forall a. a -> a
id x = x 

($) :: forall a b. (a -> b) -> a -> b
($) = id 

-- Composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)

-- Pipe (|>)
(|>) :: forall a b. a -> (a -> b) -> b
(a |> f) = f a 

-- flip
flip :: forall a b c. (a -> b -> c) -> (b -> (a -> c))
flip f b a = f a b 


-- partialApply
partialApply :: ((a, b) -> c) -> a -> (b -> c)
partialApply f x = \y -> f(x,y) 



------------------------------------------------------------------------------

-- map
map :: forall a b. (a -> b) -> [a] -> [b]
map f []     = []  
map f (x:xs) = (f x) : map f xs 

-- filter
filter :: (a -> Bool) -- test p
       -> [a]         -- values xs
       -> [a]         -- only the x in xs that satisfy p
filter p [] = []
filter p (x:xs) | p x = x : filter p xs 
                | otherwise = filter p xs 






-- dupAll
dupAll :: [a] -> [a]
dupAll xs = xs |> map (\x -> [x,x]) |> concat 




