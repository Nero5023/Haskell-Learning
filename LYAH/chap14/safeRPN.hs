import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of [(x, "")] -> Just x
                                  otherwise -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return $ (y * x):ys
foldingFunction (x:y:ys) "+" = return $ (y + x):ys
foldingFunction (x:y:ys) "-" = return $ (y - x):ys
foldingFunction xs numStr    = readMaybe numStr >>= (\x -> return $ x:xs) 
-- foldingFunction xs numStr    = liftM (:xs) $ readMaybe numStr


solveRPN :: String -> Maybe Double
solveRPN st = do
    -- Use just result to make [x:xs] this situation to Noting
    [result] <- foldM foldingFunction [] $ words st 
    return result