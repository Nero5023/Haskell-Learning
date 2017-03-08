
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