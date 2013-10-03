--(1) Detalhe os passos de reescrita envolvidos na resolução da expressão simple (simple 2 3 4) 5 6`, onde:
simple :: Integer -> Integer -> Integer -> Integer
simple a b c = a * (b + c)
-- R:
-- simple (simple 2 3 4) 5 6
-- simple (2*(3+4)) 5 6
-- (2*(3+4))*(5+6)
-- 154

-- (2) Prove, por reescrita, que simple(a−b) a b = a^2−b^2. Apenas como observação, esse tipo de demonstração é bastante semelhante a execução simbólica de um programa.
-- R:
-- simple(a−b) a b
-- (a-b)*(a+b)
-- ((a-b)*a)+((a-b)*b)
-- a^2-ab+ab-b^2
-- a^2-b^2

-- (3) Identifique quais das seguintes expressões são bem tipadas e, para cada uma delas, forneça o tipo correto:
-- • [(2,3),(4,5)]
-- • [′z′,42]
-- • (′z′,−42)
-- • simple ′a′ ′b′ ′c′ • simple(1,2,3)
-- • (simple 1 2 3,simple 4 5 6)
-- • (simple123,simple)
-- R:
-- [(2,3),(4,5)] :: [(Integer, Integer)]
-- ['z',42] - Tipagem incorreta, não pode misturar tipos na lista
-- ('z',42) :: (Char, Integer)
-- simple 'a' 'b' 'c' . simple(1,2,3) :: Tipagem incorreta, simple só aceita Integer
-- (simple 1 2 3,simple 4 5 6) :: (Integer, Integer)
-- (simple 1 2 3,simple) :: (Integer, Integer -> Integer -> Integer -> Integer)

-- (4) Prove que sumList [x] = x, para qualquer número x, onde:
sumList :: [Float] -> Float
sumList [] = 0
sumList (x:xs) = x + sumList xs
-- R:
-- sumList  [x]
-- x + sumList []
-- x + 0
-- x

-- (5) Defina uma função product :: [Float]− > Float que calcula o produtório dos elementos de uma lista. Mostre que product[2, 3, 4, 5] = 120.
produtorio :: [Float] -> Float
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs
-- produtorio[2, 3, 4, 5]
-- 2*produtorio[3, 4, 5]
-- 2*3*produtorio[4, 5]
-- 2*3*4*produtorio[5]
-- 2*3*4*5produtorio[]
-- 2*3*4*5*1
-- 120

-- (6) Escreva uma nova versão da função qSort, de tal forma que a lista resultante seja ordenada de forma decrescente. Lembrar que a implementação de qSort foi definida como:
qSort [] = []
qSort (x:xs) = qSort ys ++ [x] ++ qSort zs
      where 
      ys = [a | a <- xs, a <= x] 
      zs=[b|b<-xs,b >x]

-- R:
qSortDesc [] = []
qSortDesc (x:xs) = qSortDesc zs ++ [x] ++ qSortDesc ys
          where 
          ys = [a | a <- xs, a <= x] 
          zs=[b|b<-xs,b >x]
a = qSortDesc [2, 3, 4, 5]

-- (7) Escreva uma função em Haskell que verifica se uma lista é um palindromo. Preferencialmente, por questões de aprendizado, evite usar funções predefinidas:
-- R:
-- assinatura
palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo (x:[]) = True
palindromo (x:xs) = (x == last xs) && (palindromo (init xs))

-- exemplos de uso:
a1 = palindromo [1, 2, 3, 2, 1]
a2 = palindromo "ana"
a3 = palindromo [1, 2, 3, 4, 1, 2]

-- (9) Escreva uma função em em Haskell que elimine elementos consecutivos e repetidos de uma lista.
-- assinatura
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:xs) = if x  == head xs 
                     then compress xs
                     else x:(compress xs)

-- exemplos de uso:
b1 = compress [1, 1, 2, 3, 1]
-- [1,2,3,1]
b2 = compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- ["a","b","c","a","d","e"]

-- (10) Escreva uma função em Haskell que agrupe elementos consecutivos e repetidos em sublistas.
-- assinatura
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = if x == head xs
                 then x++pack xs
                 else [x]:(pack xs)



-- pack :: Eq a => [a] -> [[a]]
--pack [] = []
--pack (x:ys) = if x == y
--                then pack ys
--                else [x]++(pack y:ys)
--pack [] = []
--pack (x:xs) = (x : (filter (==x) xs)) : pack (filter (/=x) xs)
--pack (x:xs) = if x == head xs
--                 then (x : head xs)
--                 else [x] : pack xs

-- exemplos de uso:
--c1 = pack [ 'a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]