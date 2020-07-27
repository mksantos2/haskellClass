-- Programação Funcional - Lista 5
-- Moniele K. Santos

--numero 1

membro :: Int -> [Int] -> Bool

membro a [] = False
membro a (x:xs)
 | a == x    = True
 | otherwise = membro a xs

 --numero 2



membroNum :: Int -> [Int] -> Int

membroNum a [] = 0
membroNum a (x:xs) 
 | a == x        = 1 + (membroNum a xs)
 | otherwise     = membroNum a xs

 --numero 3


membro2 :: Int -> [Int] -> Bool

membro2 a [] = False
membro2 a (x:xs)
 | membroNum a (x:xs) > 0 = True
 | otherwise = False 


--numero 4

eliminaNum :: Int -> [Int] -> [Int]

eliminaNum a [] = []
eliminaNum a (x:xs)
 | a == x = eliminaNum a xs
 | otherwise = x: eliminaNum a xs


unico :: [Int] -> [Int]

unico [] = []
unico (x:xs)
 | (membroNum x xs) > 0 = unico (eliminaNum x xs)
 | otherwise          = x: unico xs 


--numero 5

menores :: Int -> [Int] -> [Int]

menores a [] = []
menores a (x:xs)
 | x<a       = x: menores a xs
 | otherwise = menores a xs



maiores :: Int -> [Int] -> [Int]

maiores a [] = []
maiores a (x:xs)
 | x>a       = x: maiores a xs
 | otherwise = maiores a xs



quikSort :: [Int] -> [Int]
quikSort [] = []
quikSort (x:xs) = quikSort (menores x xs)++ [x] ++quikSort (maiores x xs)

