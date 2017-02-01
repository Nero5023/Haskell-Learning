import Data.List

data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving Show

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]



heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30
                    , Section 5 90 20
                    , Section 40 2 25
                    , Section 10 8 0
                    ]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardtimeToA = timeA + a
        crosstimeToA   = timeB + c + b
        forwardtimeToB = timeB + b
        crosstimeToB   = timeA + c + a
        newPathToA     = if forwardtimeToA <= crosstimeToA
                            then (A, a):pathA
                            else (C, c):(B, b):pathB
        newPathToB    = if forwardtimeToB <= crosstimeToB
                            then (B, b):pathB
                            else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath system = 
    let (bestPathA, bestPathB) = foldl roadStep ([], []) system
        timeA = sum (map snd bestPathA)
        timeB = sum (map snd bestPathB)
    in if timeA <= timeB
        then reverse bestPathA
        else reverse bestPathB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs :  groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes     = groupsOf 3 $ map read $ lines contents
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path       = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime   = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime

