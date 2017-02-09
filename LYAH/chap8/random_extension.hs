import System.Random
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = 
    let (value, newGen) = random gen
    in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms num gen = 
    let (value, newGen) = random gen
        (rest, finalGen) = finiteRandoms (num-1) newGen
    in (value:rest, finalGen)
