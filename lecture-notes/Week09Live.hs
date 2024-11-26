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
     case runParser parseJSON resp of
       Nothing -> return "ERROR"
       Just (_, json) -> return (getString (getField json "title"))


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

-- runFetch :: Fetch a -> IO a

{-   PART 9.4 : Concurrency and Communication -}

{-
   forkIO
-}


{-  type MVar a

    newEmptyMVar :: IO (MVar a)

    putMVar :: MVar a -> a -> IO ()

    takeMVar :: MVar a -> IO a
-}

-- Logger
data LogMsg
  = Message String
  | Stop
  deriving Show

loggerMain :: MVar LogMsg -> Int -> IO ()
loggerMain inbox count =
  do msg <- takeMVar inbox
     case msg of
       Message msg ->
         do putStrLn ("LOG(" ++ show count ++ "): " ++ msg)
            loggerMain inbox (count+1)
       Stop ->
         do putStrLn "LOG STOPPED"
            return ()

startLogger :: IO (MVar LogMsg)
startLogger =
  do ch <- newEmptyMVar
     forkIO (loggerMain ch 0)
     return ch

logMsg :: MVar LogMsg -> String -> IO ()
logMsg log msg = putMVar log (Message msg)

logStop :: MVar LogMsg -> IO ()
logStop log = putMVar log Stop

type Logger = MVar LogMsg

-- doRequest
doRequest :: Logger -> Request -> IO Response
doRequest log url =
  do log `logMsg` ("Requesting " ++ url)
     httpResp <- simpleHTTP (getRequest url)
     body <- getResponseBody httpResp
     log `logMsg` ("Request " ++ url ++ " finished")
     return body

-- http://jsonplaceholder.typicode.com/todos/12

-- parMapM
parMapM :: (a -> IO b) -> [a] -> IO [b]
parMapM f xs = do
  mboxes <- mapM (\x -> do m <- newEmptyMVar
                           forkIO (do y <- f x
                                      putMVar m y)
                           return m)
                 xs
  mapM takeMVar mboxes

runFetch :: Logger -> Fetch a -> IO a
runFetch log (Done a) = return a
runFetch log (Fetch reqs k) =
  do resps <- parMapM (doRequest log) reqs
     runFetch log (k resps)

getTodo :: Int -> Fetch String
getTodo n = makeJSONRequest ("http://jsonplaceholder.typicode.com/todos/" ++ show n)

getTodos1 :: Fetch (String, String, String)
getTodos1 =
  do todo1 <- getTodo 234
     todo2 <- getTodo 123
     todo3 <- getTodo 12
     return (todo1, todo2, todo3)

getTodos2 :: Fetch (String, String, String)
getTodos2 =
  (,,) <$> getTodo 234 <*> getTodo 123 <*> getTodo 12

runFetchWithLogger :: Fetch a -> IO a
runFetchWithLogger job =
  do log <- startLogger
     result <- runFetch log job
     logStop log
     return result
