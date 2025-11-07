module Week07IO where


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) =
    do b   <- p x
       xs' <- filterM p xs
       return (if b then x:xs' else xs')

boom :: Bool
boom = boom

withLogging :: Show a => (a -> Bool) -> (a -> IO Bool)
withLogging p a = do
  _ <- print a
  return (p a)


treeSort :: Monad m
        => (a -> a -> m Bool)
        -> [a]
        -> m [a]
treeSort cmp [] = return []
treeSort cmp (v : vs) = do
  smoll <- filterM (\ w -> cmp w v) vs
  biggg <- filterM (\ w -> do { b <- cmp w v; return (not b) }) vs
  leftt <- treeSort cmp smoll
  right <- treeSort cmp biggg
  return (leftt ++ [v] ++ right)

logg :: (Show a, Show b)
     => (a -> b -> c)
     -> (a -> b -> IO c)
logg cmp a b = do
  let str = unwords ["Checking", show a, "against", show b]
  putStrLn str
  return (cmp a b)

logging
  :: Show a
  => (a -> a -> Bool)
  -> [a]
  -> IO ()
logging cmp xs = do
  _ <- treeSort (logg cmp) xs
  return ()
