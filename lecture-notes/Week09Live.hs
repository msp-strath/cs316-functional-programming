{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Live where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Traversable   (for)
import Network.HTTP       ( simpleHTTP
                          , getRequest
                          , getResponseBody
                          )
import Week08 (Parser, runParser, JSON (..), parseJSON)

{-   WEEK 09 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS -}


{-   PART 9.1 : Sequences of Actions -}

-- mapM
-- parsing with parens

-- (>>=) :: M a -> (a -> M b) -> M b

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) =
  do y  <- f x
     ys <- mapM' f xs
     return (y:ys)

data Tree a = Leaf | Node (Tree a) a (Tree a)

mapMTree :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapMTree f Leaf = return Leaf
mapMTree f (Node l x r) =
  (return Node) `ap` mapMTree f l `ap` f x `ap` mapMTree f r
         -- Node     (mapMTree f l)    (f x)    (mapMTree f r)
  -- do l' <- mapMTree f l
  --    x' <- f x
  --    r' <- mapMTree f r
  --    return (Node l' x' r')


{-   PART 9.2 : Applicative -}

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do f <- mf
              a <- ma
              return (f a)

{- class Applicative f where
     pure  :: a -> f a
     (<*>) :: f (a -> b) -> f a -> f b
        vs    (a -> f b) -> f a -> f b
-}

{-   PART 9.3 : Data Dependencies and Parallelism -}

-- Request/Reponse
type Request = String
type Response = String

-- Fetch
data Fetch a
  = Fetch [Request] ([Response] -> Fetch a)
  | Return a

instance Show (Fetch a) where
  show (Return a) = "Return"
  show (Fetch reqs _) = "Fetch " ++ show reqs ++ " <continuation>"

instance Monad Fetch where
--  return = Return

  Return a     >>= k = k a
  Fetch reqs f >>= k = Fetch reqs (\resps -> f resps >>= k)

instance Applicative Fetch where
  pure = Return

  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Return f     <*> Return a = Return (f a)
  Fetch reqs k <*> Return a =
    Fetch reqs (\resps -> k resps <*> Return a)
  Return f     <*> Fetch reqs k =
    Fetch reqs (\resps -> Return f <*> k resps)
  Fetch reqs1 k1 <*> Fetch reqs2 k2 =
    Fetch (reqs1 ++ reqs2)
          (\resps ->
             k1 (take (length reqs1) resps) <*>
             k2 (drop (length reqs1) resps))

fetch :: String -> Fetch String
fetch req = Fetch [req] (\[resp] -> Return resp)

instance Functor Fetch where
  fmap f job = pure f <*> job

{-   PART 9.4 : Concurrency and Communication -}


-- forkIO


{-  type MVar a

    newEmptyMVar :: IO (MVar a)

    putMVar :: MVar a -> a -> IO ()

    takeMVar :: MVar a -> IO a
-}

backgroundJob :: MVar String -> IO ()
backgroundJob mailbox = do
  str <- takeMVar mailbox
  putStrLn ("BACKGROUND THREAD: " ++ str)

data LogMsg
  = Log String
  | Stop
  deriving Show

logService :: MVar LogMsg -> Int -> IO ()
logService mailbox logCount =
  do msg <- takeMVar mailbox
     case msg of
       Log logMsg ->
         do putStrLn ("LOG(" ++ show logCount ++ "): " ++ logMsg)
            logService mailbox (logCount + 1)
       Stop ->
         do putStrLn "LOGGING STOPPED"

startLogger :: IO (MVar LogMsg)
startLogger = do
  mailbox <- newEmptyMVar
  forkIO (logService mailbox 0)
  return mailbox
