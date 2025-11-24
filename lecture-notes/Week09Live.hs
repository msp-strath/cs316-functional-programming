{-# LANGUAGE InstanceSigs, DataKinds, GADTs, StandaloneDeriving, RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Live where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Kind (Type)
import Data.Traversable   (for, fmapDefault, foldMapDefault)
import qualified Network.HTTP as HTTP
import Week08 (Parser, runParser, JSON (..), parseJSON)


{-    WEEK 09 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS -}

{- Part 9.1 : Sequences of Actions -}

-- (>>=) :: Monad m =>   m a  -> (a -> m b) -> m b
-- forM  :: Monad m =>  [  a] -> (a -> m b) -> m [b]

-- DISCUSS dependencies between computations


ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma =
  do f <- mf
     a <- ma
     return (f a)
-- DEFINE ap
-- DISCUSS dependencies between computations



-- DEFINE mapM_v2 :: forall m a b. Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 :: forall m a b. Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f []     = return []
mapM_v2 f (x:xs) = return (:) `ap` (f x) `ap` mapM_v2 f xs

-- using ap

-- Let's abstract over this pattern!

{- Part 9.2 : Applicative -}

-- Type class

{-
class Functor m => Applicative m where
  pure  :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b
-}

-- DEFINE mapA :: Applicative f => (a -> f b) -> [a] -> f [b]

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (x:xs)
    = pure (:) <*> f x <*> mapA f xs
--  =      (:) <$> f x <*> mapA f xs



{- Part 9.3 : Data Dependencies and Parallelism -}

-- DEFINE Request/response

newtype Request  = MkRequest { getRequest :: String }
  deriving (Show)
newtype Response = MkResponse { getResponse :: String }
  deriving (Show)

-- DEFINE Fetch monad

data Tree
  = Leaf
  | Tree :/\: Tree
  deriving Show

data Batch (t :: Tree) (a :: Type) :: Type where
  One    :: a                       -> Batch Leaf       a
  (:++:) :: Batch l a ->  Batch r a -> Batch (l :/\: r) a
deriving instance Show a => Show (Batch t a)

instance Traversable (Batch t) where
  traverse f (One a) = One <$> f a
  traverse f (l :++: r) = (:++:) <$> traverse f l <*> traverse f r

instance Foldable (Batch t) where
  foldMap = foldMapDefault

instance Functor (Batch t) where
  fmap = fmapDefault

data Fetch a where
  End :: a -> Fetch a
  Ask :: Batch t Request -> (Batch t Response -> Fetch a) -> Fetch a

-- DEFINE Show instance (to the best of our ability)
instance Show a => Show (Fetch a) where
  show (End a) = "Ended: " ++ show a
  show (Ask reqs k) = "Requests: " ++ show reqs


-- DEFINE makeRequest :: Request -> Fetch Response

makeRequest :: Request -> Fetch Response
makeRequest rq = Ask (One rq) $ \ (One rp) -> End rp

-- DEFINE Monad & Applicative instances

instance Monad Fetch where
  return = End
  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  End a >>= f = f a
  Ask rqs k >>= f = Ask rqs ((>>= f) . k)

instance Applicative Fetch where
  pure = End
  End f <*> mx = f <$> mx
  mf <*> End x = ($ x) <$> mf
  Ask rqs1 k1 <*> Ask rqs2 k2 =
    Ask (rqs1 :++: rqs2) $ \ (rsp1 :++: rsp2) ->
      k1 rsp1 <*> k2 rsp2

instance Functor Fetch where
  fmap f (End a) = End (f a)
  fmap f (Ask rsq k) = Ask rsq (fmap f . k)


runFetch
  :: Monad m
  => (forall t. Batch t Request -> m (Batch t Response))
  -> Fetch a -> m a
runFetch handle (End a) = return a
runFetch handle (Ask rqs k) = do
  rsp <- handle rqs
  runFetch handle (k rsp)


both :: Fetch (Response, Response)
both = pure (,)
  <*> makeRequest (MkRequest "github.com")
  <*> makeRequest (MkRequest "instagram.com")


runFetchIO
  :: (Request -> IO Response)
  -> Fetch a
  -> IO a
runFetchIO handleIO = runFetch $ traverse handleIO
