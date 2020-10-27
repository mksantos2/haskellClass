-- Programação Funcional - Lista 10

-- Moniele K. Santos


--numero 1


concatena :: [[a]] -> [a]

concatena [] = []

concatena (x:xs) = foldr (++) [] (x:xs) 


--numero 2


andLista :: [Bool] -> Bool

andLista [] = True 

andLista (x:xs) = foldr (&&) True (x:xs) 


--numero 3

somaQuadPos :: [Int] -> Int

somaQuadPos [] = 0

somaQuadPos (x:xs) = foldr (+) 0 (map (^2) (filter (>=0) (x:xs)))


--numero 4

somaListas :: [[Int]] -> Int

somaListas [[]] = 0

somaListas (x:xs) = foldr (+) 0 (map (sum) (x:xs))


--numero 5

tamanhoListas :: [[a]] -> Int

tamanhoListas [] = 0

tamanhoListas (x:xs) = foldr (+) 0 (map (length) ((x:xs)))


--numero 6

inverte :: [a] -> [a]

inverte (x:xs) = foldr (inverteL) [] (x:xs)
	where 
		inverteL :: a -> [a] -> [a]
		inverteL x []	= [x]
		inverteL x (y:ys) = (y:ys) ++ [x]


--numero 7

separaPalavras :: String -> [String]

separaPalavras "" = []

separaPalavras s = [takeWhile (/= ' ') s] ++ separaPalavras (tailL (dropWhile (/= ' ') s))
		where 
			tailL :: [a] -> [a]
			tailL [] = []
			tailL [a] = []
			tailL (x:xs) = xs