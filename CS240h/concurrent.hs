
module Main where

import Control.Concurrent
import Control.Exception

modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m action = do
  v0 <- takeMVar m -- -------------- oops, race condition
  (v, r) <- action v0 `onException` putMVar m v0
  putMVar m v
  return r

main = do
    putStrLn "Hello world"

type Mutex = MVar ()

mutex_create :: IO Mutex
mutex_create = newMVar()

mutex_lock, mutex_unlock :: Mutex -> IO ()
mutex_lock = takeMVar
mutex_unlock mv = putMVar mv ()

mutex_synchronize :: Mutex -> IO a -> IO a
mutex_synchronize mv action =
    bracket (mutex_lock mv) (\_ -> mutex_unlock mv)
                (\_ -> action)