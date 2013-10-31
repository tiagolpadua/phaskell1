Projeto Haskell: Primeira Parte
======================

UnB - 2o. Semestre de 2013

Departamento de Ciência da Computação - CIC

DISCIPLINA:
Linguagens de Programação

PROFESSOR:
Dr. Rodrigo Bonifácio

ALUNOS:
Tiago Lage Payne de Pádua
Eduardo Alves
Vinicius Emiliano dos Santos

As atividades implementadas estão descritas no arquivo Tests.hs


Proposta:

Evolução do Interpretador
-------------------------

A terceira versão da mini linguagem funcional implementada durante as aulas suporta operações sobre valores inteiros e booleanos, referências a expressões nomeadas, expressões Let e aplicacão de funções. Com isso, a sintaxe abstrata da linguagem é definida como:

    type Id = String
    type Args = [Exp]
    data Exp = IConst Int
    | BConst Bool
    | And Exp Exp
    | Or Exp Exp
    | Not Exp
    | Add Exp Exp;
    | Sub Exp Exp
    | Mult Exp Exp
    | Div Exp Exp
    | Let Id Exp Exp
    | RefId Id
    | App Id Args
    deriving(Show)
    
Como a linguagem suporta a aplicação ao de funções, precisamos (a) ter uma representação ao para declarar funções e (b) passar uma lista de declarações de funções para as funções que verificam os tipos (baseType) e avaliam (eval) expressões.

    data Type = IntType
    | BooleanType
    | Undefined
    deriving (Show, Eq)
    1data Value = IntValue Int
    | BooleanValue Bool
    deriving(Show, Eq)
    type FormalArgs = [Id]
    type Binding = (Id, Exp)
    type Env = [Binding]
    baseType :: Exp → Env → [FuncDecl] → Type
    ...
    eval :: Exp → Env → [FuncDecl] → Value
