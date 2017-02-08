import MyState

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \(xs) -> ((), (x:xs))


stackManip :: State Stack Int 
stackManip = do
    push 3
    a <- pop
    pop