-- Programação Funcional - Lista 12

-- Moniele K. Santos

--numero 1

data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
 deriving(Eq,Show)

arv1 :: Arvore Int
arv1 = Folha 1

arv2 :: Arvore Int
arv2 = Nodo 3 (Nodo 5 (Folha 55) (Nodo 24 (Folha 6) (Folha 3))) (Folha 5)

multDois :: Arvore Int -> Arvore Int

multDois (Folha a) = Folha (a*2)
multDois (Nodo n f1 f2) = Nodo (n*2) (multDois f1) (multDois f2)


--numero 2


contaElementos :: Arvore a -> Int

contaElementos (Folha a) = 1
contaElementos (Nodo n f1 f2) = 1 + contaElementos f1 + contaElementos f2


--numero 3 

altura :: Arvore a -> Int

altura (Folha a) = 1
altura (Nodo n f1 f2) = 1 + max (altura f1) (altura f2)


--numero 4

maiorElemento :: Arvore Int -> Int

maiorElemento (Folha a) = a
maiorElemento (Nodo n f1 f2) = max n (max (maiorElemento f1) (maiorElemento f2))

--numero 5

procuraInt :: Int -> Arvore Int -> Bool

procuraInt x (Folha a) = (x == a)
procuraInt x (Nodo n f1 f2) = (x == n) || (procuraInt x f1) || (procuraInt x f2)

--numero 6 


quantasVezes :: Int -> Arvore Int -> Int

quantasVezes x (Folha a)
 | a == x = 1
 | otherwise = 0

quantasVezes x (Nodo n f1 f2)
 | x == n = 1 + (quantasVezes x f1) + (quantasVezes x f2)
 | otherwise = (quantasVezes x f1) + (quantasVezes x f2)

 --numero 7



refleteArvore :: Arvore a -> Arvore a

refleteArvore (Folha a) = Folha a
refleteArvore (Nodo n f1 f2) = Nodo n (refleteArvore f2) (refleteArvore f1)


--numero 8


arvoreToLista :: Arvore a -> [a]

arvoreToLista (Folha a) = [a]
arvoreToLista (Nodo n f1 f2) = [n] ++ (arvoreToLista f1) ++ (arvoreToLista f2)


--numero 9 

mapTree :: (a-> b) -> Arvore a -> Arvore b

mapTree f (Folha a) = Folha (f a)
mapTree f (Nodo n f1 f2) = Nodo (f n) (mapTree f f1) (mapTree f f2)
