module MyState where

-- import Control.Monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State a) = State $ \s -> let (x, newState) = a s
                                     in (f x, newState)

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State a) = State $ \s -> let (x, newState) = a s
                                                (faTob, newState') = f newState
                                                b = faTob x
                                            in (b, newState')

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        g = f a
                                    in runState g newState 

state :: (s -> (a, s)) -> State s a
state = State