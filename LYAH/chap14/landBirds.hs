import Control.Monad.Except

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) 
    | abs ((left+n) - right) < 4 = Right (left + n, right)
    | otherwise                  = Left $ "Error " ++ show (left+n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) 
    | abs (left - (right+n)) < 4 = Right (left, right+n)
    | otherwise                  = Left $ "Error " ++ show (left, right+n)


main = do
    let x = return (0,0) >>= landLeft 1
    print x