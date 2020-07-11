-- Programação Funcional - Lista 2
-- Moniele K. Santos

--numero 1

maxi :: Int -> Int -> Int

maxi x y 
 | x>=y      = x
 | otherwise = y


 --numero 2

vendas :: Int -> Int
vendas 0 = 32
vendas 1 = 34
vendas 2 = 20
vendas 3 = 0
vendas _ = 19

maiorVenda :: Int -> Int

maiorVenda 0 = vendas 0
maiorVenda n = maxi (vendas n) (maiorVenda (n-1))


--numero 3

maxVenda :: Int -> Int

maxVenda n
 | vendas n == maiorVenda n = n
 | otherwise                = maxVenda(n-1)


--numero 4

zeroVendas :: Int -> Int

zeroVendas n 
 | n == -1       = n
 | vendas n == 0 = n
 | otherwise     = zeroVendas (n-1) 
--quando a recursao chega a zeroVendas( 0 - 1), ou seja zeroVendas(-1) e n eh igual a -1, percorreu toda as vendas e não achou alguma com valor zero. 



--numero 5

achaSemana :: Int -> Int -> Int

achaSemana s n 
 | n == -1 = n
 | s == (vendas n) = n
 | otherwise = achaSemana s (n-1) 

-- anotações:
-- s = entrada e n= numero da venda
-- se s é igual ao numero de vendas de n, retorna o n(numero da semana)
-- otherwise serve para dar o passo na recursao. Pensando em um loop, seria como um cont--. 
-- Como a recursão é feita de "baixo para cima", percorremos ao contrario como seria num loop normal. 


--numero 6

zeroVendas2 :: Int -> Int

zeroVendas2 n = achaSemana 0 n

--numero 7-a

maiorVendaMN :: Int -> Int -> Int

maiorVendaMN m n
 | m == n = vendas n 
 | otherwise = maxi (vendas n) (maiorVendaMN m (n-1))

--numero 7-b

maxVendaMN :: Int -> Int -> Int

maxVendaMN m n
 | vendas n == maiorVendaMN m n = n
 | otherwise                = maxVendaMN m (n-1)


--numero 7-c


zeroVendasMN :: Int -> Int -> Int

zeroVendasMN m n  
 | n<m       = -1
 | vendas n == 0 = n
 | otherwise     = zeroVendasMN m (n-1) 


--numero 7-d


achaSemanaMN :: Int -> Int -> Int -> Int

achaSemanaMN s m n 
 | n<m = -1
 | s == (vendas n) = n
 | otherwise = achaSemanaMN s m (n-1) 


--numero 7-d

zeroVendas2MN :: Int -> Int -> Int

zeroVendas2MN m n = achaSemanaMN 0 m n


--numero 8

fatorial :: Int -> Int 

fatorial n
 | n == 0 = 1
 | otherwise = n * fatorial(n-1)


 --numero 9

fatorialMN :: Int -> Int -> Int

fatorialMN m n 
 | m>n = 1
 | otherwise = n * fatorialMN m (n-1)


--numero 10

fib :: Int -> Int

fib n
 | n == 0 = 0
 | n == 1 = n
 | otherwise = fib(n-1)+fib(n-2)














