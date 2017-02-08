import Control.Monad.Writer
import DiffList

finalCountDownRe :: Int -> Writer (DiffList String) ()
finalCountDownRe 0 = do
    tell (toDiffList ["0"])
finalCountDownRe x = do
    finalCountDownRe $ x-1
    tell $ toDiffList [show x]

