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

-- (>>=) :: Monad m =>  m a   -> (a -> m b) -> m b
-- forM  :: Monad m =>    [a] -> (a -> m b) -> m [b]

-- DISCUSS dependencies between computations










-- ap :: Monad m => m (a -> b) -> m a -> m b

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





-- DEFINE mapA :: Applicative f => (a -> f b) -> [a] -> f [b]








{- Part 9.3 : Data Dependencies and Parallelism -}

-- DEFINE Request/response

-- DEFINE Fetch monad
data Fetch a
  = Done a
  | Fetch [Request] ([Response] -> Fetch a)


-- DEFINE Show instance (to the best of our ability)

instance Show a => Show (Fetch a) where
  show (Done a)       = "(Done " ++ show a ++ ")"
  show (Fetch reqs _) = "(Fetch " ++ show reqs ++ " <continues...>)"


-- DEFINE makeRequest :: Request -> Fetch Response


-- DEFINE Monad & Applicative instances




--

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
