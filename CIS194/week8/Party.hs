module Party where
import Employee
import Data.Tree
import Data.List

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

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glPairs = (glCons boss $ maximumGL withoutSubBossGLs, maximumGL withSubBossGls)
    where withSubBossGls    = map fst glPairs
          withoutSubBossGLs = map snd glPairs


maximumGL :: [GuestList] -> GuestList
maximumGL []  = mempty
maximumGL gls = maximum gls

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max maxPair
    where maxPair = treeFold (flip nextLevel) (mempty, mempty) tree

main = readFile "company.txt" >>= (\content -> putStrLn $ formatMaxGL $ maxFun $ read content)


formatMaxGL :: GuestList -> String
formatMaxGL (GL list fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines emps
    where emps = sort $ map empName list


-- main = do
--     content <- readFile "company.txt"
--     let datas = read content :: Tree Employee
--     -- print datas
--     print "1"
