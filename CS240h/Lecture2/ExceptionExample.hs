{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable
import System.IO.Error

data MyError = MyError String 
    deriving (Show, Typeable)

instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap (Just) action `catch` handler
    where handler (MyError msg) = do 
                putStrLn msg
                return Nothing


testThrowIO = do
    x <- throwIO (MyError "one")
    y <- throwIO (MyError "two")
    return $ x + y

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a))
                 `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
-- seqList (x:xs) b = seqList (x `seq` xs) b
seqList (x:xs) b = x `seq` seqList xs b
seqList [] b     = b

readFileIfExists f = 
    catchJust p (readFile f) (\e -> return $open show e)
        where p e = if isDoesNotExistError e then Just e else Nothing
