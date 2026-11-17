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


-- DEFINE ap
-- DISCUSS dependencies between computations



-- DEFINE mapM_v2 :: forall m a b. Monad m => (a -> m b) -> [a] -> m [b]
-- using ap

-- Let's abstract over this pattern!



{- Part 9.2 : Applicative -}

-- Type class

{-
class Functor m => Applicative m where
  pure  :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b
-}

-- DISCUSS <$>

-- DEFINE mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
--






{- Part 9.3 : Data Dependencies and Parallelism -}

-- DEFINE Request/response

-- DEFINE Batch type and Fetch monad

-- DEFINE Show instance (to the best of our ability)


-- DEFINE makeRequest :: Request -> Fetch Response

-- DEFINE Monad & Applicative instances

{-
instance Applicative Fetch where
instance Functor Fetch where
-}

-- DEFINE runFetch
-- runFect
--  :: Monad m
--  => (Batch Request -> m (Batch Response))
--  -> Fetch a -> m a

{-

both :: Fetch (Response, Response)
both = pure (,)
  <*> makeRequest (MkRequest "github.com")
  <*> makeRequest (MkRequest "instagram.com")
-}

-- DEFINE
-- runFetchIO
--   :: (Request -> IO Response)
--   -> Fetch a
--   -> IO a
