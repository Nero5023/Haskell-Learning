import Control.Monad.Writer
import DiffList

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finish with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

    


gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b 
    | b == 0 = do
        tell (toDiffList ["Finish with " ++ show a])
        return a
    | otherwise = do 
        result <- gcdReverse b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result