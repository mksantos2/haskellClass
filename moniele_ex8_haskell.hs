-- Programação Funcional - Lista 8

-- Moniele K. Santos

--numero 1

aplicaDuasVezes :: (Int->Int) -> Int -> Int

 
aplicaDuasVezes f x = f (f x)

dobra :: Int -> Int
dobra x = x*x


--numero 2

vendas :: Int -> Int
vendas 0 = 32
vendas 1 = 34
vendas 2 = 20
vendas _ = 19


vendasTotal :: (Int -> Int) -> Int -> Int

vendasTotal f 0 = f 0 
vendasTotal f x = f x + vendasTotal f (x-1)

--numero 3

foldInt :: (Int -> Int -> Int) -> [Int] -> Int

foldInt f [] = error "error"
foldInt f [a] = a
foldInt f (x:xs) = f x (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y


--numero 4

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '


filterString :: (Char -> Bool) -> [Char] -> [Char]

filterString f [] = []
filterString f (x:xs)
 | f x = [x] ++ filterString f xs
 | otherwise =  filterString f xs


 --numero 5

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs


somaQuadrado :: [Int] -> Int

somaQuadrado [] = 0
somaQuadrado (x:xs) = foldInt soma (mapInt (^2) (x:xs)) 


--numero 6

iter :: Int -> (Int -> Int) -> Int -> Int

iter 0 f 0 = 0
iter 0 f y = 0
iter x f y 
 | x == 1 = f y
 | otherwise = f (iter (x-1) f y)









