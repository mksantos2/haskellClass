-- Programação Funcional - Lista 6
-- Moniele K. Santos

--numero 1


pegaPosicao :: Int -> [Int] -> Int

pegaPosicao a [] = 0
pegaPosicao a (x:xs)
 | a == 1 = x
 | otherwise = (pegaPosicao (a-1) xs)


--numero 2

pega :: Int -> [Int] -> [Int]


pega a [] = []
pega a (x:xs)
 | a == 1 =  [x] 
 | otherwise = [x] ++ pega (a-1) xs

--numero 3

retira :: Int -> [Int] -> [Int]

retira a [] = []
retira a (x:xs)
 | a == 1  = xs
 | otherwise =  retira (a-1) xs 

 --numero 4

lenLista :: [Int] -> Int

lenLista [] = 0
lenLista (x:xs) = 1 + lenLista xs


soma :: [Int] -> Int

soma [] = 0
soma (x:xs) = x + soma xs

mediaLista :: [Int] -> Int

mediaLista [] = 0
mediaLista (x:xs) = (soma (x:xs)) `div` (lenLista(x:xs)) 

--numero 5

pegaMaiores :: Int -> [Int] -> [Int]

pegaMaiores a [] = []
pegaMaiores a (x:xs)
 | x > a = [x] ++ pegaMaiores a xs
 | otherwise = pegaMaiores a xs

--numero 6


contaMaiores :: Int -> [Int] -> Int

contaMaiores a [] = 0
contaMaiores a (x:xs)
 | x > a = 1 + contaMaiores a xs
 | otherwise = contaMaiores a xs

--numero 7

intercala :: [Int] -> [Int] -> [Int]

intercala [] [] = []
intercala [] (x:xs) = x:xs
intercala (x:xs) [] = x:xs 
intercala (x:xs) (y:ys) = [x] ++ [y] ++ intercala xs ys

--numero 8

dupli :: [Int] -> [Int]

dupli [] = []
dupli (x:xs) = x : x : dupli xs


--numero 9

duplicaChar :: Int -> [Char] -> [Char]

duplicaChar a [] = []
duplicaChar a (x:xs)
 | a == 1 = [x]
 | otherwise = [x] ++ duplicaChar (a-1) [x] 


repli :: Int -> [Char] -> [Char]

repli a [] = []
repli a (x:xs) = duplicaChar a [x] ++ repli a xs  

--numero 10

temp :: Int -> Int -> [Char] -> [Char]

temp a b [] = []
temp a b (x:xs) 
 | a == 1 = temp b b xs
 | otherwise = [x] ++ temp (a-1) b (xs)

dropEvery :: Int -> [Char] -> [Char]


dropEvery a [] = []
dropEvery a (x:xs) = temp a a (x:xs) 

--numero 11


pega1 :: Int -> [Char] -> [Char]


pega1 a [] = []
pega1 a (x:xs)
 | a == 1 =  [x] 
 | otherwise = [x] ++ pega1 (a-1) xs



retira1 :: Int -> [Char] -> [Char]

retira1 a [] = []
retira1 a (x:xs)
 | a == 1  = xs
 | otherwise =  retira1 (a-1) xs 





split :: Int -> [Char] -> ([Char],[Char])

split a [] = ( "", "")

split a (x:xs) = ((pega1 a (x:xs)), (retira1 a (x:xs)))