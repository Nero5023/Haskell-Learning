module Party where
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp{ empFun = fun}) (GL guests funs) 
    = GL (emp:guests) (fun + funs)
