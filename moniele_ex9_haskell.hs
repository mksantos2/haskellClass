-- Programação Funcional - Lista 9

-- Moniele K. Santos

--VARIAVEis

listaB = [True, False]
listaI = [1,2,3]
listaS = "moniele"




--numero 1


head :: [a] -> a

head [a] = a
head (a:x) = a

tail :: [a] -> [a]

tail [a] = [a]
tail (a:x) = x

fst :: (t, u) -> t

fst (t, u) = t

shift :: ((a,b),c) -> (a,(b,c))

shift ((a,b),c) = (a,(b,c))


--numero 2

concatena :: [[a]] -> [a]

concatena [] = []
concatena (x:xs) = x ++ concatena xs

--numero3

inverte :: [a] -> [a]

inverte [] = []
inverte (x:xs) = inverte xs ++ [x]


--numero4

zipp3 :: [a]-> [b] -> [c] -> [(a,b,c)]

zipp3 [] x y = []
zipp3 x [] y = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zipp3 xs ys zs


--numero5

mapMaisUm :: (Int -> b) -> [Int] -> [b]

mapMaisUm f [] = []
mapMaisUm f (x:xs) = f (x+1) : mapMaisUm f xs


--numero6


contaM :: Char -> Int -> Int


contaM a b
 | a == 'm' = b+1
 | otherwise = b


fol :: (a -> b -> b) -> b -> [a] -> b

fol f v [] = v
fol f v (x:xs) = f x (fol f v xs)


--explicação:

-- testei com a função contaM que recebe um char e um contador e retorna o contador com +1, caso o char seja 'm'
-- e caso contário retorne o contador se não for. Por isso o tipo de b não precisa ser necessariamente o mesmo da lista [a] 
-- e a função passada para a foldr, precisa seguir esse padrão por causa da recursividade.