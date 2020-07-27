-- Programação Funcional - Lista 3
-- Moniele K. Santos

--numero 1

somaTuplas :: ((Int, Int), (Int, Int)) -> Int

somaTuplas ((a, b), (c, d)) = a+b+c+d 

--numero 2

shift :: ((Int, Int), Int) -> (Int, (Int, Int))

shift ((a, b), c) = (a,(b,c)) 

--numero 3

maxi :: Int -> Int -> Int

maxi x y 
 | x>=y      = x
 | otherwise = y

mini :: Int -> Int -> Int

mini x y 
 | x<=y      = x
 | otherwise = y


minEmax :: Int -> Int -> Int -> (Int, Int)

minEmax a b c = ((mini(mini a b) c), maxi(maxi a b) c)


--numero 4

vendas :: Int -> Int
vendas 0 = 32
vendas 1 = 34
vendas 2 = 20
vendas 3 = 0
vendas _ = 19




zeroVenda :: Int ->(Int, Bool) 

zeroVenda n 
 | n == -1       = (-1, False)
 | vendas n == 0 = (n, True)
 | otherwise     = zeroVenda (n-1) 


--numero 5


type Livro = (String, String, String)


titulo :: Livro -> String
titulo (t, a, i) = t

autor :: Livro -> String
autor (t, a, i) = a

isbn :: Livro -> String
--O.B.S. alguns isbn possuem '-' entao estou tratando como string
isbn (t, a, i) = i


livro1 :: Livro

livro1 = ("Ensaio Sobre A Cegueira", "Saramago", "9780792755005")