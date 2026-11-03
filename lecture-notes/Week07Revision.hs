module Week07Revision where

newtype First a = MkFirst { getFirst :: Maybe a }

instance Semigroup (First a) where
  MkFirst Nothing <> r = r
  l               <> _ = l

-- laws: associative

instance Monoid (First a) where
  mempty = MkFirst Nothing

-- laws: neutral on both sides

-- give me the first element in t that satisfies
-- the (a -> Bool) test
first :: Foldable t => (a -> Bool) -> t a -> Maybe a
first test t = getFirst $ foldMap injection t
  where

   injection x =
     if test x
       then MkFirst (Just x)
       else mempty

listOf :: Foldable t => t a -> [a]
listOf = foldMap (\ x -> [x])

average :: Foldable t => t Int -> Int
average t =
  let is = listOf t in
  sum is `div` length is

newtype Count = MkCount { getCount :: Int }
instance Semigroup Count where
  MkCount m <> MkCount n = MkCount (m + n)
instance Monoid Count where
  mempty = MkCount 0

average2 :: Foldable t => t Int -> Int
average2 t =
  let injection = \ x -> (MkCount x, MkCount 1) in
  let (sum, len) = foldMap injection t in
  getCount sum `div` getCount len


count :: Foldable t => (a -> Bool) -> t a -> Int
count test t = getCount $ foldMap injection t
  where

   injection x =
     if test x
       then MkCount 1
       else mempty

all :: Foldable t => (a -> Bool) -> t a -> [a]
all test t = foldMap injection t
  where

   injection x =
     if test x
       then [x]
       else mempty

partition :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition test t = foldMap injection t
  where

   injection x =
     if test x
       then ([x], mempty)
       else (mempty, [x])
