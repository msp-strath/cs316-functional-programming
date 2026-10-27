{-# LANGUAGE DeriveFunctor #-}

module Week06Live where

-- WEEK 06 : Simulating Side Effects


--    f :: Int -> Int
--       f 0 (today) == f 0 (tomorrow)

--    f :: Int -> Effect Int


--    int f(int i)
--
--  - read the clock in the computer
--  - ask the user for input
--  - post cat picture to your favourite social network (Myspace)
--  - Launch the nuclear weapons

-- Week 06 : Simulating Side Effects
-- Week 07 : Common interface
-- Week 08 : Real I/O and side effects with the common interface



-- Simulate exceptions
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor)



find :: Eq a => a -> Tree (a,b) -> Maybe b
find k (Leaf (key,v)) | k == key = Just v
                      | otherwise = Nothing
find k (Node l r) = find k l `orElse` find k r

find2
  :: (Eq k1, Eq k2)
  => k1 -> k2
  -> (Tree (k1,Tree  (k2,v)))
  -> Maybe v
find2 k1 k2 tree = find k1 tree `andThen` (\v -> find k2 v)

  -- a -> Maybe b , b -> Maybe c
  -- Maybe b -> (b -> Maybe c) -> Maybe c

andThen :: a
andThen = undefined

orElse :: a
orElse = undefined


-- Printing
type Print a = (a, [String])

exampleTree :: Tree (Int, String)
exampleTree
  = Node (Node (Leaf (0, "Zero")) (Leaf (2, "Three")))
         (Leaf (4, "Four"))

andTHEN :: a
andTHEN = undefined

orELSE :: a
orELSE = undefined

say :: a
say = undefined

succeed :: a
succeed = undefined

failure :: a
failure = undefined

findPrint :: Eq a => a -> Tree (a,b) -> Print (Maybe b)
findPrint k (Leaf (key, v))
  | k == key =
     say "Found it!" `andTHEN` \ _ ->
     succeed v
  | otherwise =
     say "Going back up :(" `andTHEN` \ _ ->
     failure
findPrint k (Node l r) =
  findPrint k l `orELSE`
  findPrint k r

find2Print
  :: (Eq k1, Eq k2)
  => k1
  -> k2
  -> (Tree (k1, Tree (k2,v)))
  -> Print (Maybe v)
find2Print k1 k2 treeTree =
  findPrint k1 treeTree `andTHEN` \ tree ->
  findPrint k2 tree

exampleTreeTree :: Tree (Int, Tree (Int, String))
exampleTreeTree =
  Node (Leaf (2, exampleTree))
       (Leaf (5, fmap (fmap reverse) exampleTree))


-- I/O Processes
