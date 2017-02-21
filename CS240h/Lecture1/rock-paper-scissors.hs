import System.IO
import Data.Char
import Network

data Move = Rock | Paper | Scissors
    deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lost | Tie | Win deriving (Show, Eq, Ord, Read)

outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lost 

parseMove :: String -> Maybe Move
parseMove str = case reads str of [(m, "")] -> Just m
                                  _ -> Nothing

parseMove' :: String -> Maybe Move
parseMove' str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` " \r\n")

countLowerCase :: String -> Int
countLowerCase str = length (filter isLower str)

withTty :: (Handle -> IO r) -> IO r
withTty = withFile "/dev/tty" ReadWriteMode

getMove :: Handle -> IO Move
getMove h = do
    hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
    input <- hGetLine h
    case parseMove input of Just move -> return move
                            _         -> getMove h

computerVsUser :: Move -> Handle -> IO ()
computerVsUser cMove h = do
    userMove <- getMove h
    let o = outcome userMove cMove
    hPutStrLn h $ "You " ++ show o

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn = do
    s <- listenOn listenPort
    (h, host, port) <- accept s
    putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
    sClose s
    a <- fn h
    hClose h
    return a