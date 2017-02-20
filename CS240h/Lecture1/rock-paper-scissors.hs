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