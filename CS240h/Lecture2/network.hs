withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn =
  bracket (listenOn listenPort) sClose $ \s -> do
    bracket (accept s) (\(h, _, _) -> hClose h) $
      \(h, host, port) -> do
        putStrLn $ "Connection from host " ++ host
                   ++ " port " ++ show port
        fn h
