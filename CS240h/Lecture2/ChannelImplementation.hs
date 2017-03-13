import Control.Concurrent

data Item a = Item a (Stream a)
type Stream a = MVar (Item a)
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
    empty <- newEmptyMVar
    liftM2 Chan (newMVar empty) (newMVar empty)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ w) a = do
    empty <- newEmptyMVar
    modifyMVar_ w $ \oldEmpty -> do
        putMVar oldEmpty (Item a empty)
        return empty

readChan :: Chan a -> IO a
readChan (Chan r _) = 
    modifyMVar r $ \full -> do
        (Item a newFull) <- takeMVar full
        return (newFull, a)