module Week06Intro where

-- REMEMBER:
--   - Test **tomorrow** Wednesday 25th 12:00(noon) ---> Thursday 26th 12:00(noon)
--     - 10 questions on weeks 1-5
--     - should take ~1-2hrs
--     - counts for 50%
--     - redemption test in Week 9


{-    WEEK 06 : SIMULATING SIDE EFFECTS

    Haskell doesn't have "side effects" or is "pure".
     - What does this mean?
     - Is it a good thing?
     - Is it a bad thing?

    In Haskell:

       f :: Int -> Maybe Int

    what can it do?

       - Not terminate (or crash with an unrecoverable error)
       - Or it can return an Int
       - if we give it the same input twice, we'll get the same answer


    In Java:

       public static int f(int x)

    what can it do?

       - Non terminate
       - throw an Exception
       - return an int
       - print things to the screen
       - generate random numbers
       - read files
       - make network calls
         - posting cat pictures to <social network of your choice>
         - buy things on amazon
         - launch nuclear missiles

   How do we make Haskell do these things?
-}




{- Part 6.1 : Simulating Exceptions -}

{- data Maybe a = Nothing | Just a -}


returnOk :: a -> Maybe a
returnOk x = Just x

failure :: Maybe a
failure = Nothing

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure
search k ((k',v):kvs) = if k == k' then returnOk v else search k kvs

-- lookupList
lookupList :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList [] kvs = returnOk []
lookupList (k:ks) kvs =
  case search k kvs of
    Nothing -> failure
    Just v ->
      case lookupList ks kvs of
        Nothing -> failure
        Just vs -> returnOk (v:vs)

ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b
ifOK Nothing  k = failure
ifOk (Just a) k = k a


-- lookupList_v2
lookupList_v2 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList_v2 [] kvs = returnOk []
lookupList_v2 (k:ks) kvs =
  search k kvs         `ifOK` (\v ->
  lookupList_v2 ks kvs `ifOK` (\vs ->
  returnOk (v:vs)))

-- ";"
-- MyType a = <statement1>; <statement2>

catch :: Maybe a -> Maybe a -> Maybe a
catch Nothing  handler = handler
catch (Just a) handler = Just a

safeLookupList :: Eq k => [k] -> [(k,v)] -> Maybe [v]
safeLookupList ks kvs = catch (lookupList_v2 ks kvs) (returnOk [])







{- Part 6.2 : Simulating (Mutable) State -}

{- We can make updatable state 'pure' by making fresh names for
   variables instead of treating each variable as a thing that can
   change.

   Instead of:

     int i = 0;

     i = 10;

     i = i + 1;

     ...

     i = i - 1;

   Make fresh variables:

     int i0 = 0;

     int i1 = 10;

     int i2 = i1 + 1;

     ...

     int i3 = i2 - 1;

  This is the form that compilers use internally when compiling most
  languages SSA (Static Single Assignment). Used in (e.g.) LLVM and
  GCC.
-}

{- LinkedList<> output = new LinkedList<Pair<Int,String>>();
   int i = 0;
   for (String x : xs) {
      Pair<> p = new Pair(i, x);
      output.append(p);
      i++;
   }
-}

numberList :: [a] -> Int -> (Int, [(a, Int)])
numberList [] i = (i, [])
numberList (x:xs) i0 =
  let p = (x, i0)
      i1 = i0 + 1
      (i2, ys) = numberList xs i1
  in (i2, p : ys)


type State a = Int -> (Int, a)

returnSt :: a -> State a
         -- a -> Int -> (Int,a)
returnSt a i = (i,a)

andThen :: State a          -> (a -> State b)         -> State b
        -- (Int -> (Int,a)) -> (a -> Int -> (Int, b)) -> Int -> (Int, b)
andThen computation1 kontinuation i0 =
  let (i1, a) = computation1 i0
      (i2, b) = k a i1
  in (i2, b)

get :: State Int
get i = (i,i)

put :: Int -> State ()
put i i0 = (i, ())

numberList_v2 :: [a] -> State [(a,Int)]
numberList_v2 [] = returnSt []
numberList_v2 (x:xs) =
  -- get              `andThen` \i ->
  -- put (i+1)        `andThen` \() ->
  increment        `andThen` \i ->
  numberList_v2 xs `andThen` \ys ->
  returnSt ((x,i) : ys)

increment :: State Int
increment = get `andThen` \i -> put (i+1) `andThen` \() -> returnSt i


-- returnOk :: a -> Maybe a
-- returnSt :: a -> State a

-- ifOK    :: Maybe a -> (a -> Maybe b) -> Maybe b
-- andThen :: State a -> (a -> State b) -> State b
