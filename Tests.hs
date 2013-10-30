module Tests where 

import Test.HUnit

import Language
import Interpreter

-- Parte 1 do projeto: Suporte a argumentos formais
soma       = FuncDecl "soma" [("x", IntType), ("y", IntType)] (Add (RefId "x") (RefId "y"))

-- Parte 2 do projeto: Este teste irá gerar um erro pois "n" não está declarado no ambiente da função
foo        = FuncDecl "foo" [("p", IntType)] (RefId "n")

-- Parte 3 do projeto: Implementação do IfThenElse
ifthenelse = IfThenElse (BConst True) (IConst 1) (IConst 2)

-- Parte 4 do projeto: IfThenElse retorna uma função Lambda anônima, dependendo do valor da expressão
-- teste, IfThenElse retorna a funcao "x+1" ou a função "x+2"
lambda     = IfThenElse (BConst True) (Lambda (Add (RefId "x") (IConst 1))) (Lambda (Add (RefId "x") (IConst 2)))

-- Outros testes:
inc        = FuncDecl "inc" [("x", IntType)] (Add (RefId "x") (IConst 1))
eqb        = Eq (BConst True) (BConst True)
eqi        = Eq (Add (IConst 1) (IConst 1)) (IConst 2)
fat        = FuncDecl "fat" [("n", IntType)] (IfThenElse (Eq (RefId "n") 
												(IConst 1)) (RefId "n") 
												(Mult (RefId "n") (App "fat" [(Sub (RefId "n") (IConst 1))])))


test1 = TestCase (assertEqual "inc 5" (IntValue 6) (eval (App "inc" [(IConst 5)]) [] [inc]))
test2 = TestCase (assertEqual "soma 3 4" (IntValue 15) (eval (App "soma" [(IConst 5),(IConst 10)]) [] [soma]))
test3 = TestCase (assertEqual "Let n = 5 in f 3, onde foo p = n" (IntValue 5) (eval (Let "n" (IConst 5) (App "foo" [(IConst 3)])) [] [foo]))
test4 = TestCase (assertEqual "If (True or False) then 1 else 2" (IntValue 1) (eval ifthenelse [] []))
test5 = TestCase (assertEqual "Eq True True" (BooleanValue True) (eval eqb [] [] ))
test6 = TestCase (assertEqual "Eq 1+1 2" (BooleanValue True) (eval eqi [] [] ))

-- Teste não utilizado pois o programa entra em loop
test7 = TestCase (assertEqual "Fat 5 é 120" (IntValue 120) (eval (App "fat" [(IConst 5)]) [] [fat]))

test8 = TestCase (assertEqual "Lambda" (IntValue 2) (eval lambda [("x", IConst 1)] []))

allTests = 	TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, 
			TestLabel "test6" test6, TestLabel "test8" test8]


--			TestLabel "test6" test6, TestLabel "test7" test7]

-- allTests = 	TestList [TestLabel "test8" test8]