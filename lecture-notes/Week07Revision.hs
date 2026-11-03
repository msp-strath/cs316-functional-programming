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

-- give me the list of elements in t
listOf :: Foldable t => t a -> [a]
listOf = foldMap (\ x -> [x])

-- give me the average of values in t
average :: Foldable t => t Int -> Int
average t =
  let is = listOf t in
  sum is `div` length is

newtype Count = MkCount { getCount :: Int }
instance Semigroup Count where
  MkCount m <> MkCount n = MkCount (m + n)
instance Monoid Count where
  mempty = MkCount 0

-- give me the average of values in t,
-- but better thanks to Monoids
average2 :: Foldable t => t Int -> Int
average2 t =
  let injection = \ x -> (MkCount x, MkCount 1) in
  let (sum, len) = foldMap injection t in
  getCount sum `div` getCount len

-- give me the number of values in t that
-- satisfy the (a -> Bool) predicate
count :: Foldable t => (a -> Bool) -> t a -> Int
count test t = getCount $ foldMap injection t
  where

   injection x =
     if test x
       then MkCount 1
       else mempty

-- give me all the values in t that
-- satisfy the (a -> Bool) predicate
all :: Foldable t => (a -> Bool) -> t a -> [a]
all test t = foldMap injection t
  where

   injection x =
     if test x
       then [x]
       else mempty

-- group the values in t into lists of
-- values that respectively satisfy
-- and not satisfy the (a -> Bool) predicate
partition :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition test t = foldMap injection t
  where

   injection x =
     if test x
       then ([x], mempty)
       else (mempty, [x])
