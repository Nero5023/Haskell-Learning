data Tree a = Empty
            | Node (Tree a) a (Tree a)

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize :: Tree a -> Integer
treeSize = treeFold 0 (\l _ r -> 1 + l + r)

treeSum :: Tree Integer -> Integer
treeSum = treeFold 0 (\l x r -> x + l + r)

treeDepth :: Tree a -> Integer
treeDepth = treeFold 0 (\l x r -> 1 + max l r)

flatten :: Tree a -> Integer
flatten = treeFold [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)


data ExprT = Lit Integer
            | Add ExprT ExprT 
            | Mul ExprT ExprT

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i) = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval :: ExprT -> Integer
eval = exprTFold id (+) (*)

numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)