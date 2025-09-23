{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Live where

type Coin = Int

makeChange :: [Coin]         -- coins that are available
           -> [Coin]         -- coins used so far
           -> Int            -- target amount
           -> Maybe [Coin]   -- coins that add up to the target (maybe)
makeChange available        used 0 = Just used
makeChange []               _    n = Nothing
makeChange (coin:available) used n
  | n >= coin = makeChange available (coin:used) (n - coin)
  | otherwise = makeChange available used n

correctChange :: [Coin] -> Int -> Bool
correctChange available target = case makeChange available [] target of
  Nothing -> True
  Just used -> sum used == target

makeChange_v2 :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange_v2 available        used 0 = Just used
makeChange_v2 []               used n = Nothing
makeChange_v2 (coin:available) used n
  | n >= coin = case makeChange_v2 available (coin:used) (n - coin) of
      Just change -> Just change
      Nothing -> makeChange_v2 available used n
  | otherwise = makeChange_v2 available used n

------------------------------------------------------------------------------

success :: a -> Maybe a
success x = Just x

failure :: Maybe a
failure = Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y

makeChange_v3 :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange_v3 available        used 0 = success used
makeChange_v3 []               used n = failure
makeChange_v3 (coin:available) used n
  | n >= coin =
      makeChange_v3 available (coin:used) (n - coin)
      `orElse`
      makeChange_v3 available used n
  | otherwise = makeChange_v3 available used n

--  myFunc(f(), g())

successL :: a -> [a]
successL x = [x]

failureL :: [a]
failureL = []

orElseL :: [a] -> [a] -> [a]
orElseL xs ys = xs ++ ys

makeChange_v4 :: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChange_v4 available        used 0 = successL used
makeChange_v4 []               used n = failureL
makeChange_v4 (coin:available) used n
  | n >= coin =
      makeChange_v4 available (coin:used) (n - coin)
      `orElseL`
      makeChange_v4 available used n
  | otherwise = makeChange_v4 available used n

------------------------------------------------------------------------------

data Choices a
  = Success a
  | Failure
  | Choose (Choices a) (Choices a)
  deriving Show

successC :: a -> Choices a
successC x = Success x

failureC :: Choices a
failureC = Failure

orElseC :: Choices a -> Choices a -> Choices a
orElseC xs ys = Choose xs ys

makeChange_v5 :: [Coin] -> [Coin] -> Int -> Choices [Coin]
makeChange_v5 available        used 0 = successC used
makeChange_v5 []               used n = failureC
makeChange_v5 (coin:available) used n
  | n >= coin =
      makeChange_v5 available (coin:used) (n - coin)
      `orElseC`
      makeChange_v5 available used n
  | otherwise = makeChange_v5 available used n

greedy :: Choices a -> Maybe a
greedy (Success x)  = Just x
greedy Failure      = Nothing
greedy (Choose x y) = greedy x

firstChoice :: Choices a -> Maybe a
firstChoice (Success x)  = Just x
firstChoice Failure      = Nothing
firstChoice (Choose x y) = firstChoice x `orElse` firstChoice y

allChoices :: Choices a -> [a]
allChoices (Success x)  = [x]
allChoices Failure      = []
allChoices (Choose x y) = allChoices x ++ allChoices y

best :: (a -> Int) -> Choices a -> Maybe a
best cost (Success x) = Just x
best cost Failure     = Nothing
best cost (Choose x y) =
  case (best cost x, best cost y) of
    (Nothing, Nothing) -> Nothing
    (Just x,  Nothing) -> Just x
    (Nothing, Just y)  -> Just y
    (Just x,  Just y)  ->
      if cost x <= cost y then Just x else Just y
