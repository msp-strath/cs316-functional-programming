module Week06Live where

-- REMINDER: Class Test:
--             Wednesday 30th October 12:00 noon ---> Thursday 31st October 12:00 noon
--             Test will be via MyPlace
--             Test is worth 50% and marked out of 50

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

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving Show

find :: Eq a => a -> Tree (a, b) -> Maybe b
find k (Leaf (k', v))
  | k == k' = Just v
  | otherwise = Nothing
find k (Node l r) = case find k l of
  Just v -> Just v
  Nothing -> find k r

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing k = Nothing
andThen (Just v) k = k v

find2 :: (Eq k1, Eq k2)
      => k1 -> k2
      -> Tree (k1, Tree (k2, a)) -> Maybe a
find2 k1 k2 t =
  find k1 t `andThen` \ t2 -> find k2 t2

-- Tree t1 = find(k1, t);
-- return find(k2, t1)

returnOk :: a -> Maybe a
returnOk x = Just x

findAll :: Eq k => Tree (k, v) -> [k] -> Maybe [v]
findAll dictionary [] = Just []
findAll dictionary (k:ks) =
  find k dictionary     `andThen` \ v ->
  findAll dictionary ks `andThen` \ vs ->
  returnOk (v : vs)


-- State

-- int i = 0;
--
-- i = i + 1;

type State s a = s -> (a, s)

andThenState :: State s a
             -> (a -> State s b)
             -> State s b
andThenState c k initial =
  let (a, intermediate) = c initial in
  k a intermediate

returnState :: a -> State s a
returnState v s = (v, s)

getState :: State s s
getState s = (s, s)

putState :: s -> State s ()
putState new old = ((), new)

numberTree :: Tree a -> State Int (Tree (a, Int))
numberTree (Leaf a)   =
    getState `andThenState` \ i ->
    putState (i + 1) `andThenState` \ _ ->
    returnState (Leaf (a, i))
numberTree (Node l r) =
    numberTree l `andThenState` \ numbered_l ->
    numberTree r `andThenState` \ numbered_r ->
    returnState (Node numbered_l numbered_r)
--  let (numbered_l, i1) = numberTree l i0
--      (numbered_r, i2) = numberTree r i1
--  in (Node numbered_l numbered_r, i2)

example = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- andThen      :: Maybe   a -> (a -> Maybe   b) -> Maybe   b
-- andThenState :: State s a -> (a -> State s b) -> State s b

-- Choice a -> Choice a -> Choice a

-- andThenChoice :: Choice a -> (a -> Choice b) -> Choice b
