{-# LANGUAGE InstanceSigs, DataKinds, GADTs, StandaloneDeriving, RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Live where

import Data.Functor (void)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Kind (Type)
import Data.Traversable (for, fmapDefault, foldMapDefault)
import qualified Network.HTTP as HTTP
import Week08 (Parser, runParser, JSON (..), parseJSON)

import qualified Debug.Trace as Debug

------------------------------------------------------------------------
-- WEEK 09: DATA DEPENDENCIES and APPLICATIVE FUNCTORS
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Batches of requests and responses indexed by their shape

newtype Request  = MkRequest  { getRequest  :: String } deriving (Show)
newtype Response = MkResponse { getResponse :: String } deriving (Show)

data Tree = Leaf | Tree :/\: Tree deriving Show

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

------------------------------------------------------------------------
-- A shape-respecting Request/Response interaction tree

data Fetch a where
  End :: a -> Fetch a
  Ask :: Batch t Request -> (Batch t Response -> Fetch a) -> Fetch a

instance Show a => Show (Fetch a) where
  show (End a) = "Ended: " ++ show a
  show (Ask reqs k) = "Requests: " ++ show reqs

makeRequest :: Request -> Fetch Response
makeRequest rq = Ask (One rq) $ \ (One rp) -> End rp

instance Monad Fetch where
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

-- Silly example
both :: Fetch (Response, Response)
both = pure (,)
  <*> makeRequest (MkRequest "github.com")
  <*> makeRequest (MkRequest "instagram.com")

------------------------------------------------------------------------
-- A (simple) interpretation function

runFetch
  :: Monad m
  => (forall t. Batch t Request -> m (Batch t Response))
  -> Fetch a -> m a
runFetch handle (End a) = return a
runFetch handle (Ask rqs k) = do
  rsp <- handle rqs
  runFetch handle (k rsp)

runFetchIO
  :: (Request -> IO Response)
  -> Fetch a
  -> IO a
runFetchIO handleIO = runFetch $ traverse handleIO


------------------------------------------------------------------------
-- WEEK 10: CONCURRENCY

-- First, a big code dump to do some experiments

------------------------------------------------------------------------
-- Observing that runFetchIO is still quite sequential

handleHTTP :: Request -> IO Response
handleHTTP req = do
  res <- HTTP.simpleHTTP (HTTP.getRequest (getRequest req))
  MkResponse <$> HTTP.getResponseBody res


-- Check out http://jsonplaceholder.typicode.com/todos/12

getTodo :: Int -> Fetch String
getTodo n
  = makeJSONRequest
  $ MkRequest
  $ "http://jsonplaceholder.typicode.com/todos/" ++ show n

getField :: JSON -> String -> JSON
getField (Object fields) nm =
  case lookup nm fields of
    Nothing -> Null
    Just v  -> v
getField _ _ =
  Null

getString :: JSON -> String
getString (String s) = s
getString _          = "ERROR"

makeJSONRequest :: Request -> Fetch String
makeJSONRequest url =
  do resp <- makeRequest url
     case runParser parseJSON (getResponse resp) of
       Nothing -> return "ERROR"
       Just (_, json) -> return (getString (getField json "title"))

getTodos1 :: Fetch (String, String, String)
getTodos1 =
  do todo1 <- getTodo 234
     todo2 <- getTodo 123
     todo3 <- getTodo 12
     return (todo1, todo2, todo3)

getTodos2 :: Fetch (String, String, String)
getTodos2 =
  (,,) <$> getTodo 234 <*> getTodo 123 <*> getTodo 12

-- Bigger examples

todos :: [Int]
todos = [1..50]

getTodosA :: Fetch [String]
getTodosA = traverse getTodo todos

getTodosM :: Fetch [String]
getTodosM = mapM getTodo todos

-- QUESTION: is there a difference between getTodosA & getTodosM?


------------------------------------------------------------------------
-- Let's actually run concurrent threads!

{- The magic ingredients:

-- Forks
   forkIO :: IO () -> IO ()

-- MVars

   type MVar a
   newEmptyMVar :: IO (MVar a)
   putMVar :: MVar a -> a -> IO ()
   takeMVar :: MVar a -> IO a
-}



------------------------------------------------------------------------
-- First: some logging machinery

oopsy :: IO ()
oopsy = do
  forkIO (putStrLn "Hello World, I'm 1")
  putStrLn "Hello World, I'm 2"

-- Can't just print willy-nilly
-- 1 logger thread logging messages one by one
-- Other threads sending their messages to it

data LogMsg = Message String | Stop
  deriving Show

type Mailbox = MVar LogMsg

loggerMain :: Mailbox -> Int -> IO ()
loggerMain inbox count = do msg <- takeMVar inbox
                            case msg of
                                Message msg -> do  putStrLn ("LOG ( " ++ show count ++ " ) : " ++ msg)
                                                   loggerMain inbox (count + 1)
                                Stop        -> do  putStrLn "STOPPED"
                                                   return()

-- Going on forever? :(

startLogger :: IO Mailbox
startLogger = do ch <- newEmptyMVar
                 forkIO (loggerMain ch 0)
                 return ch

logMsg :: Mailbox -> String -> IO ()
logMsg log msg = putMVar log (Message msg)

logStop :: Mailbox -> IO ()
logStop log = putMVar log Stop


withLogger :: (Mailbox -> IO b) -> IO b
withLogger k = do
  log <- startLogger
  result <- k log
  logStop log
  return result

-- RUN some experiments

handleHTTPwithLogger :: Mailbox -> Request -> IO Response
handleHTTPwithLogger log url =
  do log `logMsg` ("Requesting " ++ getRequest url)
     body <- handleHTTP url
     log `logMsg` ("Request " ++ getRequest url ++ " finished")
     return body

runFetchwithLogger :: Fetch a -> IO a
runFetchwithLogger job = withLogger $ \ inbox ->
  runFetchIO (handleHTTPwithLogger inbox) job


------------------------------------------------------------------------
-- Next: a parallel mapM

-- parTraverse
parTraverse :: Traversable t => (a -> IO b) -> t a -> IO (t b)
parTraverse f t = do
  mboxes <- flip traverse t $ \x -> do
    m <- newEmptyMVar
    forkIO (putMVar m =<< f x)
    return m
  traverse takeMVar mboxes

parRunFetchwithLogger :: Fetch a -> IO a
parRunFetchwithLogger job = withLogger $ \ inbox ->
  runFetch (parTraverse (handleHTTPwithLogger inbox)) job
