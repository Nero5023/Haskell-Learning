module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Read, Show, Ord)

instance Monoid Score where
    mempty  = Score 0
    mappend (Score l) (Score r) = Score (l+r)

score :: Char -> Score
score ch
    | chLow `elem` "aeilnorstu" = Score 1
    | chLow `elem` "dg"         = Score 2
    | chLow `elem` "bcmp"       = Score 3
    | chLow `elem` "fhvwy"      = Score 4
    | chLow `elem` "k"          = Score 5
    | chLow `elem` "jx"         = Score 8
    | chLow `elem` "qz"         = Score 10
    | otherwise              = Score 0
        where chLow = toLower ch

scoreString :: String -> Score
scoreString = foldl (\acc x -> acc <> score x) $ Score 0 


getScore :: Score -> Int
getScore (Score a) = a
