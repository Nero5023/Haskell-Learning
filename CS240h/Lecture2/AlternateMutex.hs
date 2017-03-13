
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

type Mutex = MVar ThreadId
mutex_create :: IO Mutex
mutex_create = newEmptyMVar

mutex_lock mv = myThreadId >>= putMVar mv

mutex_lock mv = do 
    mytid <- myThreadId
    lockTid <- tryTakeMVar mv
    unless (lockTid == Just mytid) $
        error "mutex_unlock"

main = putStrLn "Hello World"