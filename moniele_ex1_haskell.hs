-- Programação Funcional - Lista 1
-- Moniele K. Santos


maiorIdade :: Int -> Bool

maiorIdade idade = (idade>=18)
 


--numero 1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool

osQuatroSaoIguais x y z w = x==y && x==w && y==z && y==w && w==z


--numero 2

quantosSaoIguais :: Int -> Int -> Int -> Int 

quantosSaoIguais x y z 
 | x==y && y==z = 3
 | x==z         = 2
 | z==y         = 2
 | y==x         = 2
 | otherwise    = 0


--numero 3 

todosDiferentes :: Int -> Int -> Int -> Bool

todosDiferentes x y z = x/=y && x/=z && y/=z

--numero 4(descritiva)

--O que esta errado com a seguinte definicao de todosDiferentes:
--todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
--O conjunto de testes que voce definiu na questao anterior funciona com
--esta definicao?

-- R: Essa definição não testa se n e p são diferentes. E não funciona com testes como n=1 m=0 e p=1. 


--numero 5

todosIguais :: Int -> Int -> Int -> Bool

todosIguais x y z = x==y && x==z 

quantosSaoIguais2 :: Int -> Int -> Int -> Int

quantosSaoIguais2 x y z
 | todosIguais x y z     = 3
 | todosDiferentes x y z = 0
 | otherwise             = 2

--numero 7

elevadoQuatro :: Int -> Int

elevadoQuatro n = n * n 


-- numero 8

vendas :: Int -> Int
vendas 0 = 32
vendas 1 = 34
vendas 2 = 20
vendas _ = 19


vendasTotal :: Int -> Int

vendasTotal 0 = vendas 0
vendasTotal n = vendas n + vendasTotal n-1


