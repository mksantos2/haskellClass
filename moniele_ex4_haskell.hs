-- Programação Funcional - Lista 4
-- Moniele K. Santos

--numero 1

multDoisLista :: [Int] -> [Int]

multDoisLista [] = []
multDoisLista (x : xs) = 2*x : multDoisLista xs

--numero 2

tamanho :: [Int] -> Int

tamanho [] = 0
tamanho (x:xs) =  1 + tamanho xs


--numero 3

produtoLista :: [Int] -> Int

produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs


--numero 4

andLista :: [Bool] -> Bool

andLista [x] = x
andLista (x:xs) = x && andLista xs


--numero 5

concatLista :: [[Int]] ->[Int]

concatLista [x] = x
concatLista (x:xs) = x ++ concatLista xs

--numero 6

inverteLista :: [Int] -> [Int]

inverteLista [x] = [x]
inverteLista (x:xs) = inverteLista xs ++ [x] 


