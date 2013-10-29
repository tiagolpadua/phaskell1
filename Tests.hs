module Tests where 

import Test.HUnit

import Language
import Interpreter

inc        = FuncDecl "inc" [("x", IntType)] (Add (RefId "x") (IConst 1))
soma       = FuncDecl "soma" [("x", IntType), ("y", IntType)] (Add (RefId "x") (RefId "y"))
foo        = FuncDecl "foo" [("p", IntType)] (RefId "n")
ifthenelse = (IfThenElse (BConst True) (IConst 1) (IConst 2))
eqb        = (Eq (BConst True) (BConst True))
eqi        = (Eq (Add (IConst 1) (IConst 1)) (IConst 2))
fat        = FuncDecl "fat" [("n", IntType)] (IfThenElse (Eq (RefId "n") 
												(IConst 1)) (RefId "n") 
												(Mult (RefId "n") (App "fat" [(Sub (RefId "n") (IConst 1))])))


test1 = TestCase (assertEqual "inc 5" (IntValue 6) (eval (App "inc" [(IConst 5)]) [] [inc]))
test2 = TestCase (assertEqual "soma 3 4" (IntValue 15) (eval (App "soma" [(IConst 5),(IConst 10)]) [] [soma]))
test3 = TestCase (assertEqual "Let n = 5 in f 3, onde foo p = n" (IntValue 5) (eval (Let "n" (IConst 5) (App "foo" [(IConst 3)])) [] [foo]))
test4 = TestCase (assertEqual "If (True or False) then 1 else 2" (IntValue 1) (eval ifthenelse [] []))
test5 = TestCase (assertEqual "Eq True True" (BooleanValue True) (eval eqb [] [] ))
test6 = TestCase (assertEqual "Eq 1+1 2" (BooleanValue True) (eval eqi [] [] ))
test7 = TestCase (assertEqual "Fat 5 é 120" (IntValue 120) (eval (App "fat" [(IConst 5)]) [] [fat]))

allTests = 	TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, 
			TestLabel "test6" test6]
--			TestLabel "test6" test6, TestLabel "test7" test7]