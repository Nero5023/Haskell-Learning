import Control.Monad
main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors tahta you assocaite with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
    