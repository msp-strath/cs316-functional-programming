{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Live where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
--import Prelude hiding (mapM)
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
        -- Fetch a -> (a -> Fetch b) -> Fetch b
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

makeRequest :: String -> Fetch String
makeRequest req = Fetch [req] (\[resp] -> Return resp)

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

type Logger = MVar LogMsg

startLogger :: IO Logger
startLogger = do
  mailbox <- newEmptyMVar
  forkIO (logService mailbox 0)
  return mailbox

logMessage :: Logger -> String -> IO ()
logMessage logger msg =
  putMVar logger (Log msg)

logStop :: Logger -> () -> IO ()
logStop logger () =
  putMVar logger Stop



-- http://jsonplaceholder.typicode.com/todos/12



-- Executing Requests concurrently

doRequest :: Logger -> Request -> IO Response
doRequest log url =
  do log `logMessage` ("Requesting " ++ url)
     httpResp <- simpleHTTP (getRequest url)
     body <- getResponseBody httpResp
     log `logMessage` ("Request " ++ url ++ " finished")
     return body

parMapM :: (a -> IO b) -> [a] -> IO [b]
parMapM f xs =
  do mailboxes <-
       mapM (\a -> do m <- newEmptyMVar
                      forkIO (do b <- f a
                                 putMVar m b)
                      return m)
            xs
     mapM takeMVar mailboxes

runFetch :: Logger -> Fetch a -> IO a
runFetch log (Return a) = return a
runFetch log (Fetch reqs k) =
  do resps <- parMapM (doRequest log) reqs
     runFetch log (k resps)

getField :: JSON -> String -> JSON
getField (Object fields) nm =
  case lookup nm fields of
    Nothing -> Null
    Just x  -> x
getField _ nm = Null

getString :: JSON -> String
getString (String s) = s
getString _          = "ERROR"

getTodo :: Int -> Fetch String
getTodo id =
  do json <- makeRequest ("http://jsonplaceholder.typicode.com/todos/" ++ show id)
     case runParser parseJSON json of
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
  pure (\todo1 todo2 todo3 -> (todo1, todo2, todo3))
  <*> getTodo 234
  <*> getTodo 123
  <*> getTodo 12

runFetchWithLogger :: Fetch a -> IO a
runFetchWithLogger job =
  do log <- startLogger
     result <- runFetch log job
     log `logStop` ()
     return result
