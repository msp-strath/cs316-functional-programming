{-# LANGUAGE InstanceSigs, DataKinds, GADTs, StandaloneDeriving, RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Live where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Kind (Type)
import Data.Traversable   (for, fmapDefault, foldMapDefault)
import qualified Network.HTTP as HTTP
import Week08 (Parser, runParser, JSON (..), parseJSON)






-- ADMIN:
-- EPSRC Paid Vacation Internships 2027
-- Projects are 10 weeks long and distinct from the normal
-- UG degree courses
--
-- If you are interested contact us so we can discuss a project.
-- Note this is a competitive process





{-    WEEK 09 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS
      See https://doi.org/10.1017/S0956796807006326
-}

{- Part 9.1 : Sequences of Actions -}

-- (>>=) :: Monad m =>   m a  -> (a -> m b) -> m b
-- forM  :: Monad m =>  [  a] -> (a -> m b) -> m [b]

-- DISCUSS dependencies between computations

myforM :: Monad m => [a] -> (a -> m b) -> m [b]
myforM [] k = return []
myforM (a:as) k = do
  b <- k a
  bs <- myforM as k
  return (b:bs)

-- DEFINE ap
-- DISCUSS dependencies between computations

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  return (f x)


-- DEFINE mapM_v2 :: forall m a b. Monad m => (a -> m b) -> [a] -> m [b]
-- using ap
myforM2 :: Monad m => [a] -> (a -> m b) -> m [b]
myforM2 [] k = return []
myforM2 (a:as) k = return (:) `ap` k a `ap` myforM2 as k

-- Let's abstract over this pattern!

{- Part 9.2 : Applicative -}

-- Type class

{-
class Functor m => Applicative m where
  pure  :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b

-- laws:
-- pure f <*> mx     === fmap f mx
-- mf     <*> pure x === fmap ($ x) mf
-- pure f <*> pure x === pure (f x)
-}

-- DISCUSS <$>
-- <$> = fmap
-- f <$> mx === pure f <*> mx

-- DEFINE mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
forA :: Applicative f => [a] -> (a -> f b) -> f [b]
forA [] k = pure []
forA (a:as) k
  = pure (:)     -- :: f (b -> [b] -> [b])
  <*> k a        -- :: f  b
  <*> forA as k  -- :: f       [b]
  ------------------------------------
                 -- :: f              [b]


{- Part 9.3 : Data Dependencies and Parallelism -}

-- DEFINE Request/response

newtype Request = MkRequest { getRequest :: String }
  deriving (Show)
newtype Response = MkResponse { getResponse :: String }
  deriving (Show)

-- DEFINE Batch type and Fetch monad
data Batch a
  = One a
  | Both (Batch a) (Batch a)
 deriving (Show)

-- DEFINE Show instance (to the best of our ability)
data Fetch a
  = Pure a
  | Ask (Batch Request) (Batch Response -> Fetch a)

-- DEFINE makeRequest :: Request -> Fetch Response

makeRequest :: Request -> Fetch Response
makeRequest r = Ask (One r) (\ (One a) -> Pure a)

-- DEFINE Monad & Applicative instances

instance Applicative Fetch where
  pure = return
  Pure f <*> mx = fmap f mx
  mf <*> Pure x = fmap ($ x) mf
  Ask rs1 k1 <*> Ask rs2 k2 =
    Ask (Both rs1 rs2) $ \ (Both as1 as2) -> k1 as1 <*> k2 as2

instance Functor Fetch where
  fmap f (Pure x) = Pure (f x)
  fmap f (Ask rs k) = Ask rs (\ as -> fmap f (k as))

instance Monad Fetch where
  return = Pure

  Pure x >>= lk = lk x
  Ask rs k >>= lk = Ask rs (\ as -> k as >>= lk)


instance Show a => Show (Fetch a) where
  show (Pure x) = show x
  show (Ask rs k) = show rs ++ "..."

example :: Fetch (Response, Response)
example = pure (,)
  <*> makeRequest (MkRequest "https://github.com")
  <*> makeRequest (MkRequest "https://classes.myplace.strath.ac.uk")

-- DEFINE runFetch
-- runFect
--  :: Monad m
--  => (Batch Request -> m (Batch Response))
--  -> Fetch a -> m a


-- DEFINE
-- runFetchIO
--   :: (Request -> IO Response)
--   -> Fetch a
--   -> IO a
