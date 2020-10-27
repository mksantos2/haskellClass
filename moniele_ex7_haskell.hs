-- Programação Funcional - Lista 7
-- Moniele K. Santos

--numero 1

somaQuadrupla :: [(Int, Int, Int, Int)] -> Int

somaQuadrupla [] = 0
somaQuadrupla ((a,b,c,d):xs) = a + b + c + d + somaQuadrupla xs

--numero 2

somaTuplas :: [((Int, Int), (Int, Int))] -> Int

somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xs) = a + b + c + d + somaTuplas xs

--numero 3

zipp :: [Int] -> [Int] -> [(Int, Int)]

zipp [] [] = []
zipp [] (x:xs) = []
zipp (x:xs) [] = [] 
zipp (x:xs) (y:ys) = [(x, y)] ++ zipp xs ys

--numero 4

zipTres :: [Int] -> [Int] ->  [Int] -> [(Int, Int, Int)]

zipTres [] [] []= []
zipTres [] (x:xs) []= []
zipTres (x:xs) [] [] = [] 
zipTres (x:xs) (y:ys) [] = []
zipTres [] [] (x:xs) = []
zipTres [] (y:ys) (x:xs) = []
zipTres (y:ys) [] (x:xs)= []

zipTres (x:xs) (y:ys) (z:zs) = [(x, y, z)] ++ zipTres xs ys zs

--numero 5

unzippEsq :: [(Int,Int)] -> [Int]

unzippEsq [] = []
unzippEsq ((a,b):xs) = [a] ++ unzippEsq xs

unzippDir :: [(Int,Int)] -> [Int]

unzippDir [] = []
unzippDir ((a,b):xs) = [b] ++ unzippEsq xs


unZipp :: [(Int, Int)] -> ([Int], [Int])


unZipp ((a,b):xs) = (unzippEsq ((a,b):xs), unzippDir ((a,b):xs) )
