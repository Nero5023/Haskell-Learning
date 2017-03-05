factorial n0 = loop 1 n0
    where loop acc n 
            | n > 1     = loop (acc * n) (n-1)
            | otherwise = acc

factorial_seq n0 = loop 1 n0
    where loop acc n
            | n > 1     = acc `seq` loop (acc * n) (n-1)
            | otherwise = acc