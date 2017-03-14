import Control.Concurrent

data Cond = Cond (MVar [MVar ()])

cond_create :: IO Cond
cond_create = liftM Cond $ newMVar []
-- liftM is fmap for Monads (though today all Monads are Functors anyway):
-- liftM f m1 = do x <- m1; return (f m1)

cond_wait :: Mutex -> Cond -> IO ()
cond_wait m (Cond waiters) = do
  me <- newEmptyMVar
  modifyMVar_ waiters $ \others -> return $ others ++ [me]
  mutex_unlock m   -- note we don't care if preempted after this
  takeMVar me `finally` mutex_lock m
  
cond_signal, cond_broadcast :: Cond -> IO ()
cond_signal (Cond waiters) = modifyMVar_ waiters wakeone
    where wakeone [] = return []
          wakeone (w:ws) = putMVar w () >> return ws

cond_broadcast (Cond waiters) = modifyMVar_ waiters wakeall
    where wakeall ws = do mapM_ (flip putMVar ()) ws
                          return []
