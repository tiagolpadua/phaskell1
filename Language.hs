module Language where 

data Type = IntType | BooleanType | Undefined
 deriving (Show, Eq)

data Value = IntValue Int
           | BooleanValue Bool
   deriving(Show, Eq)

type Id = String 

type FormalArgs = [(Id,Type)]
type Args = [Exp]
type Binding = (Id, Exp)

type Env = [Binding]

data FuncDecl = FuncDecl Id FormalArgs Exp

data Exp = IConst Int
         | BConst Bool
	       | And Exp Exp
	       | Or Exp Exp
	       | Not Exp
	       | Add Exp Exp
	       | Sub Exp Exp
         | Mult Exp Exp  
         | Div Exp Exp
         | Eq Exp Exp
         | Let Id Exp Exp 
         | RefId Id
         | App Id Args 
         | IfThenElse Exp Exp Exp
 deriving(Show)


findBoundExp :: Id -> Env -> [Exp]
findBoundExp ref env = [e | (r, e) <- env, r == ref] 

findFuncDecls :: Id -> [FuncDecl] -> [FuncDecl] 
findFuncDecls n fds = [f | f@(FuncDecl fn fargs exp) <- fds, n == fn]
 
baseType :: Exp -> Env -> [FuncDecl] -> Type
baseType (IConst v) _ _ = IntType
baseType (BConst v) _ _ = BooleanType
baseType (And lhs rhs) env fds = sameTypes (lhs, rhs) BooleanType env fds
baseType (Or lhs rhs) env fds = sameTypes (lhs, rhs) BooleanType env fds
baseType (Add lhs rhs) env fds = sameTypes (lhs, rhs) IntType env fds
baseType (Sub lhs rhs) env fds = sameTypes (lhs, rhs) IntType env fds
baseType (Mult lhs rhs) env fds = sameTypes (lhs, rhs) IntType env fds
baseType (Div lhs rhs) env fds = sameTypes (lhs, rhs) IntType env fds
baseType (Not e) env fds = sameType e BooleanType env fds
--baseType (Eq lhs rhs) env fds = sameTypes (lhs, rhs)  env fds

-- o tipo base de uma referencia a um 
-- id corresponde ao tipo base da expressao 
-- associada ao mesmo id no ambiente env. Caso
-- o identificador nao esteja associado a uma 
-- expressao, o tipo eh indefinido (erro).
baseType (RefId i) env fds = 
 let exps = findBoundExp i env 
 in case exps of 
     [] -> Undefined
     (x:xs) -> baseType x env fds

-- na nossa implementacao, o tipo base de uma aplicacao de funcao
-- consiste em verificar se a quantidade de 
-- argumentos passados na aplicacao da funcao eh 
-- igual a quantidade de argumentos formais usados 
-- na declaracao de funcao. Caso afirmativo, verificamos 
-- se a expressao do corpo da funcao eh bem tipada em um novo 
-- amibente que associa os argumentos formais aos argumentos 
-- passados na chamada da funcao.  
baseType (App n args) env fds = 
 case findFuncDecls n fds of 
  []  -> error $ "Function " ++ n ++ " not declared"
--  [f@(FuncDecl fn fargs exp)] -> let env' = (zip fargs args) ++ env in baseType exp env' fds
  [f@(FuncDecl fn fargs exp)] -> 
    if validaTipoArgs (map snd fargs) args env fds
--    then let env' = (zip (map fst fargs) args) ++ env in baseType exp env' fds
    then let env' = (zip (map fst fargs) args) in baseType exp env' fds
    else error $ "Arguments with invalid types"
  (f1:f2:fs) -> error $ "Multiple declarations of " ++ n      

-- o tipo base de uma expressao let verifica 
-- inicialmente se o tipo base da expressao associada 
-- ao identificador eh Undefined. Caso afirmativo, 
-- o tipo da expressao let como um todo corresponde ao 
-- a Undefined. Caso contrario (o tipo da expressao assiciada 
-- ao identificador eh diferente de Undefined), o tipo base 
-- da expressao let eh o tipo base da expressao que corresponde 
-- ao corpo da expressao let.
baseType (Let _ exp1 exp2) env fds = 
  case baseType exp1 env fds of 
    Undefined -> Undefined
    otherwise -> baseType exp2 env fds

-- O tipo base de Eq verifica se os tipos das duas expressões comparadas 
-- são iguais
baseType (Eq lhs rhs) env fds  =
  case (baseType lhs env fds) == (baseType rhs env fds) of
    True -> BooleanType

-- primeiro verificamos se os tipos das expressões para o Then e o Else são os mesmos,
-- se forem, então testamos se a expressão teste é BooleanType
baseType (IfThenElse expTeste expThen expElse) env fds =
  case (baseType expThen env fds) == (baseType expElse env fds) of
    True -> sameType expTeste BooleanType env fds
    otherwise -> Undefined

-- Valida os tipos dos argumentos até que não hajam mais argumentos
-- a serem validados, retornando então True, caso contrário gera um erro
validaTipoArgs :: [Type] -> Args -> Env -> [FuncDecl] -> Bool
validaTipoArgs [] _ _ _ = True
validaTipoArgs (t:ts) (e:es) env fds = if t == (baseType e env fds)
                                      then validaTipoArgs ts es env fds
                                      else error $ "Arguments type error"

-- Uma funcao auxiliar usada para checar se 
-- dois operandos de uma expressao possuem o 
-- mesmo tipo, retornando esse tipo caso verdadeiro e 
-- Undefined caso contrario.
sameTypes :: (Exp, Exp) -> Type -> Env -> [FuncDecl] -> Type
sameTypes (lhs, rhs) t env fds
 | (baseType lhs env fds == t) && (baseType rhs env fds == t) = t
 | otherwise = Undefined
 

-- Uma funcao auxiliar usada para checar se 
-- um operando possui um determinado tipo, 
-- retornando esse tipo caso verdadeiro e Undefined 
-- caso contrario.
sameType :: Exp -> Type -> Env -> [FuncDecl] -> Type
sameType e t env fds
 | (baseType e env fds == t) = t
 | otherwise = Undefined