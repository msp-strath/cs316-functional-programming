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

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just v) _ = Just v
orElse Nothing ma = ma

find2
  :: (Eq k1, Eq k2)
  => k1 -> k2
  -> (Tree (k1,Tree  (k2,v)))
  -> Maybe v
find2 k1 k2 tree = find k1 tree `andThen` (\v -> find k2 v)

-- a bit like
-- v = find k1 tree;
-- find k2 v

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just v) k = k v










-- Printing

exampleTree :: Tree (Int, String)
exampleTree
  = Node (Node (Leaf (0, "Zero")) (Leaf (2, "Three")))
         (Leaf (4, "Four"))

type Print a = (a, [String])

andTHEN :: Print (Maybe a) -- Print (Maybe a) = (Maybe a, [String])
        -> (a -> Print (Maybe b))
        -> Print (Maybe b)
andTHEN (Nothing, msg) k = (Nothing, msg)
andTHEN (Just a, msg)  k = let (mb, msg') = k a in (mb, msg ++ msg')
{- equivalent to:
 case k a of
   (mb, msg') -> (mb, msg ++ msg')
-}

orELSE :: Print (Maybe a) -> Print (Maybe a) -> Print (Maybe a)
--orELSE (Just a, msg) _ = (Just a, msg)
orELSE happy@(Just _, _) _ = happy
orELSE (Nothing, msg) (ma, msg') = (ma, msg ++ msg')

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
  (say "Going Left" `andTHEN` \ _ ->
    indented 2 $ findPrint k l)
    `orELSE`
  (say "Going Right" `andTHEN` \ _ ->
     indented 2 $ findPrint k r)
{- With more printing!

  (say "Going Left" `andTHEN` \ _ -> findPrint k l)
  `orELSE`
  (say "Going Right" `andTHEN` \ _ -> findPrint k r)
-}
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



{-
--(Int,Int,str)
myFilter p xs = foldr cons [] xs where

  -- (\x acc -> if p x then x:acc else acc)
  cons x acc = if p x then x:acc else acc
-}

-- DISCUSS e.g.

indented :: Int -> Print a -> Print a
indented n (a, ls) = (a, padLWith (replicate n ' ') ls) where

  padLWith :: Semigroup a => a -> [a] -> [a]
  padLWith padding [] = []
  padLWith padding (hd:tl) = (padding <> hd) : padLWith padding tl

  -- (1+) is the same as (\ x -> 1+x)
  -- padLWith padding
  --  = map (\ x -> padding <> x)
  --  = map (padding <>)


-- I/O Processes


type InputOutput a
  = [String]             -- all of the inputs
  -> ( Print a           -- printing some outputs
     , [String])         -- returning the inputs not used

ioSay :: String -> InputOutput ()
ioSay str = \ inputs -> (((), [str]), inputs)

ioGet :: InputOutput String
ioGet (line : rest) = ((line, []), rest)

ioSuccess :: a -> InputOutput a
ioSuccess x = \ lines -> ((x, []), lines)

ioThen :: InputOutput a
       -> (a -> InputOutput b)
       -> InputOutput b
ioThen ioa kb lines0 =
  let ((a, msg1), lines1) = ioa lines0 in
  let ((b, msg2), lines2) = kb a lines1 in
  ((b, msg1 ++ msg2), lines2)


echo :: InputOutput ()
echo = ioGet `ioThen` ioSay

spooky :: (((), [String]), [String])
spooky = echo ["Boooh", "Coooh"]
