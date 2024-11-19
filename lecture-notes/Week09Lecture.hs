{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Lecture where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Traversable   (for)
import Network.HTTP       ( simpleHTTP
                          , getRequest
                          , getResponseBody
                          )
import Week08 (Parser, runParser, JSON (..), parseJSON)


{-    WEEK 09 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS -}

{- Part 9.1 : Sequences of Actions -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return (y : ys)

-- (>>=) : Monad m => m a -> (a -> m b) -> m b

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  return (f a)

-- mapM_v2 :: forall m a b. Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_v2 f []       = return []
-- mapM_v2 f (x : xs)
--   = (return (:)     :: m (b -> [b] -> [b]))
--     `ap` (f x       :: m b)
--     `ap` (mapM f xs :: m [b])
                   -- (:) (f x) (map f x)

{-
class Functor m => Applicative m where
  pure  :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b
-}

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f [] = pure []
mapA f (x : xs) = (:) <$> f x <*> mapA f xs


{- Part 9.2 : Applicative -}

-- Type class

{- Part 9.3 : Data Dependencies and Parallelism -}

-- Request/response
type Request = String
type Response = String

-- Fetch
data Fetch a
  = Done a
  | Fetch [Request] ([Response] -> Fetch a)

instance Show a => Show (Fetch a) where
  show (Done a)       = "(Done " ++ show a ++ ")"
  show (Fetch reqs _) = "(Fetch " ++ show reqs ++ " <continues...>)"

makeRequest :: Request -> Fetch Response
makeRequest url = Fetch [url] (\[resp] -> Done resp)

-- Monad
instance Monad Fetch where
  Done x       >>= k = k x
  Fetch reqs c >>= k = Fetch reqs (\resps -> c resps >>= k)

-- Applicative
instance Applicative Fetch where
  pure = Done
  Done f <*> Done x = Done (f x)
  Done f <*> Fetch reqsr cr
    = Fetch reqsr (\ respr -> fmap f (cr respr))
  Fetch reqsl cl <*> Done x
    = Fetch reqsl (\ respl -> fmap (\ f -> f x) (cl respl))
  Fetch reqsl cl <*> Fetch reqsr cr
    = Fetch (reqsl ++ reqsr) (\ resplr ->
       let (respl, respr) = splitAt (length reqsl) resplr in
       let left = cl respl in
       let right = cr respr in
       left <*> right)

instance Functor Fetch where
  fmap f mx = pure f <*> mx

-- Functor

-- runFetch :: Fetch a -> IO a
