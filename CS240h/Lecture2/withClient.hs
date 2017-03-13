import Control.Exception
import Network
import System.IO

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient portid handler = do
    bracket (listenOn portid) sClose $ \s -> do
        bracket (accept s) (\(h,_,_) -> hClose h) $ \(h, host, port) -> do
            putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
            handler h

