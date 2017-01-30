import System.IO

main = do 
    handle <- openFile "haiku.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
