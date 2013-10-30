module Interpreter where 

import Language

-- Nosso interpretador recebe uma expressao
-- e retorna um valor. 
--teste
eval :: Exp -> Env -> [FuncDecl] -> Value   
eval (IConst x) _ _ = IntValue x
eval (BConst x) _ _ = BooleanValue x

-- avalia a expressao exp2 em um ambiente 
-- env' atualizado, que adiciona a associacao 
-- (i, exp1) ao ambiente env. 
eval (Let i exp1 exp2) env fds = 
 let 
  env' = (i, exp1) : env 
 in eval exp2 env' fds

-- avalia a expressao associada ao identificador 
-- ref (no ambiente env). Caso nao tenha uma associacao, 
-- uma excecao eh lancada.
eval (RefId ref) env fds = 
 let exps = findBoundExp ref env
 in case exps of 
     []  -> error $ " not in scope " ++ ref 
     (x:xs) -> eval x env fds

-- avalia a expressão App
eval (App n args) env fds = 
 case findFuncDecls n fds of 
  []  -> error $ "Function " ++ n ++ " not declared"
  [(FuncDecl fn fargs exp)] -> 
--    let env' = (zip (map fst fargs) args) ++ env 
--    in eval exp env' fds
    if (validaTipoArgs (map snd fargs) args env fds)
    then
      let env' = zip (map fst fargs) args
      in eval exp env' fds
    else
      error $ "Invalid parameters: " ++ n    
  (f1:f2:fs) -> error $ "Multiple declarations of " ++ n    

-- avalia a expressão Lambda
eval e@(Lambda exp) env fds = 
  let btLambda = (baseType e env fds) in
    if btLambda == BooleanType || btLambda == IntType 
    then
      eval exp env fds
    else error $ "Wrong datatypes in " ++ (show e) 

-- avalia a expressão IfThenElse
eval e@(IfThenElse expTeste expThen expElse) env fds =
  let btIfThenElse = (baseType e env fds) in
     if btIfThenElse == BooleanType || btIfThenElse == IntType 
     then
       case (eval expTeste env fds) of
         (BooleanValue True) -> eval expThen env fds
         otherwise -> eval expElse env fds
     else error $ "Wrong datatypes in " ++ (show e) 

-- avalia a expressão Eq
eval e@(Eq lhs rhs) env fds =
  case (baseType e env fds) of
  (BooleanType) ->  case (eval lhs env fds) == (eval rhs env fds) of
                    True -> (BooleanValue True)
                    False -> (BooleanValue False)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 


-- a avaliacao de expressoes booleanas / aritmeticas 
-- envolve a checagem de tipos. Mas isso foi delegado 
-- para as funcoes auxiliares evalBinBooleanExp e 
-- evalBinIntExp. 
eval e@(Add  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (+) env fds
eval e@(Sub  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (-) env fds 
eval e@(Mult lhs rhs) env fds = evalBinIntExp e (lhs, rhs) (*) env fds
eval e@(Div  lhs rhs) env fds = evalBinIntExp e (lhs, rhs) div env fds
eval e@(And  lhs rhs) env fds = evalBinBooleanExp e (lhs, rhs) (&&) env fds
eval e@(Or   lhs rhs) env fds = evalBinBooleanExp e (lhs, rhs) (||) env fds
eval e@(Not   exp) env fds    = 
 case (baseType e env fds) of
  (BooleanType) -> let
                    (BooleanValue v) = eval exp env fds
                   in BooleanValue (not v)
  otherwise -> error $ "Wrong datatypes in " ++ (show e) 

-- Possivel simplificar ainda mais!!!! Trabalho para 
-- o aluno Andre ou Anayran.
evalBinBooleanExp :: Exp -> (Exp, Exp) -> (Bool -> Bool -> Bool) -> Env -> [FuncDecl] -> Value
evalBinBooleanExp e (lhs, rhs) op env fds = 
 case (baseType e env fds) of
  (BooleanType) -> let
                    (BooleanValue l) = eval lhs env fds 
                    (BooleanValue r) = eval rhs env fds 
                   in BooleanValue (l `op` r)
  otherwise -> error $ "evalBinBooleanExp: Wrong datatypes in " ++ (show e) 


evalBinIntExp :: Exp -> (Exp, Exp) -> (Int -> Int -> Int) -> Env -> [FuncDecl] -> Value
evalBinIntExp e (lhs, rhs) op env fds = 
 case (baseType e env fds) of
  (IntType) -> let
                    (IntValue l) = eval lhs env fds 
                    (IntValue r) = eval rhs env fds
                   in IntValue (l `op` r)
  otherwise -> error $ "evalBinIntExp: Wrong datatypes in " ++ (show e) ++ " - " ++ (show (baseType e env fds)) 
                     
