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

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _  = Nothing
andThen (Just v) f = f v

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just v) alt = Just v
orElse Nothing alt = alt



-- Printing
type Print a = (a, [String])

exampleTree :: Tree (Int, String)
exampleTree
  = Node (Node (Leaf (0, "Zero")) (Leaf (2, "Three")))
         (Leaf (4, "Four"))

andTHEN
  :: Print (Maybe a)
  -> (            a -> Print (Maybe b))
  ->                   Print (Maybe b)
andTHEN (Nothing, msg) f = (Nothing, msg)
andTHEN (Just v, msg1) f = case f v of
  (mb, msg2) -> (mb, msg1 ++ msg2)

orELSE
  :: Print (Maybe a)
  -> Print (Maybe a)
  -> Print (Maybe a)
orELSE (Just v, msg) _ = (Just v, msg)
orELSE (Nothing, msg1) (ma, msg2) = (ma, msg1 ++ msg2)

say :: String -> Print (Maybe ())
say str = (Just (), [str])

succeed :: a -> Print (Maybe a)
succeed v = (Just v, [])

failure :: Print (Maybe a)
failure = (Nothing, [])

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
