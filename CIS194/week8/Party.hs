module Party where
import Employee
import Data.Tree

-- Exercise 1
-- 1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp{ empFun = fun}) (GL guests funs) 
    = GL (emp:guests) (fun + funs)

-- 2
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL list0 fun0) (GL list1 fun1)
        = GL (list0 ++ list1) $ fun0 + fun1

-- 3
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl0@(GL _ fun0) gl1@(GL _ fun1)
    | fun0 >= fun1 = gl0
    | otherwise    = gl1

-- Exercise 2
treeFold :: ([b] -> a -> b) -> b -> Tree a -> b
treeFold f init (Node label trees) 
    = f (map (treeFold f init) trees) label