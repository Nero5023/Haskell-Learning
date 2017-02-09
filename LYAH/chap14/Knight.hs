import Data.List
import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1)
                , (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')


inMany :: Int -> KnightPos -> [KnightPos]
inMany n start = return start >>= foldr (<=<) return (replicate n moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start end = end `elem` inMany n start