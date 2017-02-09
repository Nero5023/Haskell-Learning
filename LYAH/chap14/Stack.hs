import MyState

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \(xs) -> ((), (x:xs))


stackManip :: State Stack Int 
stackManip = do
    a <- pop
    push a
    push a
    push a
    pop