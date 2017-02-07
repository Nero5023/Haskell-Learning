{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import System.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dieN :: Int -> Rand StdGen [DieValue]
dieN n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } 
    deriving Show


-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = dieN (att+def) >>= \dvs -> 
        return $ lostArmy bf $ battleOutcom (take att dvs) (drop att dvs)
    where (att, def) = getADArm bf

-- get the attacks and defends for the Battlefield
getADArm :: Battlefield -> (Army, Army)
getADArm (Battlefield att def) = (att', def')
    where att' = if att > 5 then 3 else att-1
          def' = if def > 2 then 2 else def

-- Return the loss of armys for attackers and defenders
-- args :: the first is the attackers dice values
--         the second is the defenders dice values
battleOutcom :: [DieValue] -> [DieValue] -> (Army, Army)
battleOutcom atts defs = (elemNum False cmps, elemNum True cmps)
    where atts' = sortBy (flip compare) atts
          defs' = sortBy (flip compare) defs
          cmps  = zipWith (>) atts' defs'

elemNum :: (Eq a) => a -> [a] -> Int
elemNum element
    = foldl (\ acc x -> if x == element then acc + 1 else acc) 0 

-- get the battlefied and the lost the arms for attackers and defends 
-- compute Battlefied after the lost
lostArmy :: Battlefield -> (Army, Army) -> Battlefield
lostArmy (Battlefield att def) (lostAtt, lostDef) 
    = Battlefield (att - lostAtt) (def - lostDef)

-- Exercise 3
--  simulates an entire invasion attempt
invade :: Battlefield -> Rand StdGen Battlefield
invade bf 
    | attackers bf < 2 || defenders bf < 1 = return bf
    | otherwise = battle bf >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= probability

-- get the probability of the attckers wins
probability :: [Battlefield] -> Rand StdGen Double
probability bfs = return $ fromIntegral (length attWins) / fromIntegral (length bfs)
    where attWins = filter ((==0) . defenders) bfs


main = do
    values <- evalRandIO (successProb $ Battlefield 5 5)    
    putStrLn $ show (values*100) ++ "%"