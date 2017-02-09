import Data.Ratio
import Data.List

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- flatten :: Prob (Prob a) -> Prob a
-- flatten (Prob xs) = Prob 

instance Applicative Prob where
    pure = return
    fa <*> xs = do
        f <- fa
        x <- xs
        return $ f x


mulInner :: (Prob a, Rational) -> [(a, Rational)]
mulInner (Prob inner, p) = map (\(x, p') -> (x, p*p')) inner

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map mulInner xs

thisSituation = Prob [
    (Prob [('a', 1%2), ('b', 1%2)], 1%4),
    (Prob [('c', 1%2), ('d', 1%2)], 3%4)]


instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f  = flatten $ fmap f m
    fail _   = Prob []

joinElements :: (Eq a) => [(a, Rational)] -> (a, Rational) -> [(a, Rational)]
joinElements xs pair@(a, p) = if a `elem` map fst xs 
                              then map (\(x, r) -> if x == a then (a, p+r) else (x, r)) xs 
                              else pair:xs

simpProb :: (Eq a) => Prob a -> Prob a
simpProb (Prob xs) = Prob $ foldl joinElements [] xs