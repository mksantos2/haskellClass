import Data.Char
import System.IO

tabuleiroP1 :: [String]
tabuleiroP1 = ["14","p1"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]

tabuleiroP2 :: [String]
tabuleiroP2 = ["14","p2"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]

auxP1 :: [String]
auxP1 = ["14","p1"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]

auxP2 :: [String]
auxP2 = ["14","p1"," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "]
--10x10


-- Leituras de nomes, linahs e colunas


leNome1 :: IO String
leNome1 = do 
 putStrLn  "Qual o nome do player 1?"
 nome <- getLine
 return nome

leNome2 :: IO String
leNome2 = do 
 putStrLn  "Qual o nome do player 2?"
 nome <- getLine
 return nome

getLinha :: IO Int
getLinha = do
 putStrLn "Escolha uma linha de A a I: "
 letra <- getLine
 return (retornaInt (fromEnum (primChar letra)))

getColuna :: IO Int
getColuna = do
 putStrLn "Escolha uma coluna de 1 a 9: "
 coluna <- getLine
 return ((fromEnum (primChar coluna)) - 48)

getCoordenada :: IO Int
getCoordenada = do
 linha <- getLinha
 coluna <- getColuna
 return (linha + coluna)

primChar :: [Char] -> Char
primChar [] = '0'
primChar (x:xs) = x

getOrientacao :: IO String
getOrientacao = do
 putStrLn "Horizontal ou vertical?"
 orientacao <- getLine
 return orientacao

-- Leitura das coordenadas
-- leCoordenada, leCoordenadaSub, leCoordenadaCruz, leCoordenadaEnc e leCoordenadaPA

leCoordenada :: IO Int
leCoordenada = do
 linha <- getLinha
 coluna <- getColuna
 limpaTela
 return (linha + coluna + 2)

leCoordenadaSub :: [String] -> IO [Int]
leCoordenadaSub l = do
 putStrLn "\n------- Primeiro Submarino -------"
 soma1 <- getCoordenada
 limpaTela
 putStr(printTabuleiro 83 (navios [soma1] l))
 putStrLn "\n------- Segundo Submarino -------"
 soma2 <- getCoordenada
 limpaTela
 putStr(printTabuleiro 83 (navios ([soma1]++[soma2]) l))
 putStrLn "\n------- Terceiro Submarino -------"
 soma3 <- getCoordenada
 let submarinos = soma1 : soma2 : [soma3]
 limpaTela
 putStr(printTabuleiro 83 (navios submarinos l))
 return submarinos

leCoordenadaCruz :: [String] -> IO [Int]
leCoordenadaCruz l = do
 putStrLn "\n------- Primeiro Cruzador -------"
 orientacao <- getOrientacao
 soma <- getCoordenada
 let soma1 = if orientacao == "horizontal"
 	then soma : [soma + 1]
 	else soma : [soma + 9]
 limpaTela
 putStr(printTabuleiro 83 (navios soma1 l))
 putStrLn "\n------- Segundo Cruzador -------"
 orientacao <- getOrientacao
 soma <- getCoordenada
 let soma2 = if orientacao == "horizontal"
 	then [soma] ++ [(soma + 1)]
 	else [soma] ++ [(soma + 9)]
 let cruzadores = soma1 ++ soma2
 limpaTela
 putStr(printTabuleiro 83 (navios cruzadores l))
 return cruzadores

leCoordenadaEnc :: [String] -> IO [Int]
leCoordenadaEnc l = do
 putStrLn "\n------- Encouracado -------"
 orientacao <- getOrientacao
 soma <- getCoordenada
 let encouracado = if orientacao == "horizontal"
 	then soma : (soma + 1) : [soma + 2]
 	else soma : (soma + 9) : [soma + 18]
 limpaTela
 putStr(printTabuleiro 83 (navios encouracado l))
 return encouracado

leCoordenadaPA :: [String] -> IO [Int]
leCoordenadaPA l = do
 putStrLn "\n------- Porta Aviões -------"
 orientacao <- getOrientacao
 soma <- getCoordenada
 let portaAviao = if orientacao == "horizontal"
 	then soma : (soma + 1) : (soma + 2) : [soma + 3]
 	else soma : (soma + 9) : (soma + 18) : [soma + 27]
 limpaTela
 putStr(printTabuleiro 83 (navios portaAviao l))
 return portaAviao

-- Printa o tabuleiro com a formatação do tabuleiro de Batalha Naval

printTabuleiro :: Int -> [String] -> String
printTabuleiro 0 l = "\n"
printTabuleiro n (x:xs)
 |n == 83 = printTabuleiro (n-1) xs
 |n == 82 = "Tabuleiro de: " ++ x ++ "\n" ++ printTabuleiro (n-1) xs
 |n == 81 = "    1 2 3 4 5 6 7 8 9\n A |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 72 = "\n B |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 63 = "\n C |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 54 = "\n D |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 45 = "\n E |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 36 = "\n F |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 27 = "\n G |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 18 = "\n H |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |n == 9 = "\n I |" ++ x ++ "|" ++ printTabuleiro (n-1) xs
 |otherwise =  x ++ "|" ++ printTabuleiro (n-1) xs

-- Coloca todos os navios no tabuleiro

colocaNavios :: [String] -> IO [String]
colocaNavios l = do
 submarinos <- leCoordenadaSub l
 if submarinos == []
  then return l
  else do
 let tabuleiro = navios submarinos l 
 cruzadores <- leCoordenadaCruz tabuleiro
 if cruzadores == []
  then return l
  else do
 let tabuleiro2 = navios cruzadores tabuleiro
 encouracado <- leCoordenadaEnc tabuleiro2
 if encouracado == []
  then return l
  else do
 let tabuleiro3 = navios encouracado tabuleiro2
 portaAviao <- leCoordenadaPA tabuleiro3
 if portaAviao == []
  then return l
  else do
 let tabuleiroFinal = navios portaAviao tabuleiro3
 return tabuleiroFinal

colocaNavio :: Int -> [String] -> [String]
colocaNavio 0 (x:xs) = "N" : xs
colocaNavio n [] = []
colocaNavio n (x:xs) = x : colocaNavio (n-1) (xs)

navios :: [Int] -> [String] -> [String]
navios [] l = l
navios (x:xs) l = navios xs (colocaNavio (x+1) l)

-- Coloca o nome do dono do tabuleiro

colocaNome :: String -> [String] -> [String]
colocaNome [] l = l
colocaNome l [] = []
colocaNome l (x:s:xs) = x : l : xs

-- Retorna o valor equivalente a letra com relação a sua posição no tabuleiro

retornaInt :: Int -> Int
retornaInt 0 = 0 
retornaInt n 
 |n == 97 || n == 65 = 0 --a ou A
 |n == 98 || n == 66 = 9 --b ou B 
 |n == 99 || n == 67 = 18 --c ou C
 |n == 100 || n == 68 = 27 --d ou D
 |n == 101 || n == 69 = 36 --e ou E
 |n == 102 || n == 70 = 45 --f ou F
 |n == 103 || n == 71 = 54 --g ou G
 |n == 104 || n == 72 = 63 --h ou H
 |n == 105 || n == 73 = 72 --i ou I
 |otherwise = 0

comecaJogo :: Int -> [String] -> [String] -> [String] -> [String] -> IO Int
comecaJogo v a a1 b b1 = do
 if isZero a1
 	then return 2
 	else do
 if isZero b1
 	then return 1
 	else do
 if v == 1
 	then do
 		let player = jogador a
 		putStr(printTabuleiro 83 b1)
 		putStrLn ("\nVez do jogador " ++ player)
 		putStrLn ("\nNavios restantes: "++ (primeiro b1))
 		b2 <- ataque b b1
 		let v = 2
 		vencedor <- comecaJogo v a a1 b b2
 		return vencedor
 	else do
 		let player = jogador b
 		putStr(printTabuleiro 83 a1)
 		putStrLn ("\nVez do jogador " ++ player)
 		putStrLn ("\nNavios restantes: "++ (primeiro a1))
 		a2 <- ataque a a1
 		let v = 1
 		vencedor <- comecaJogo v a a2 b b1
 		return vencedor

isZero :: [String] -> Bool
isZero [] = True
isZero (x:xs)
 |x == "0" = True
 |otherwise = False

primeiro :: [String] -> String
primeiro (x:xs) = x

valor :: [String] -> Int
valor (x:xs) = ord (primChar x)

jogador :: [String] -> String
jogador [] = ""
jogador (x:s:xs) = s

menosUm :: String -> String
menosUm [] = ""
menosUm x 
 |x == "14" = "13"
 |x == "13" = "12"
 |x == "12" = "11"
 |x == "11" = "10"
 |x == "10" = "9"
 |x == "9" = "8"
 |x == "8" = "7"
 |x == "7" = "6"
 |x == "6" = "5"
 |x == "5" = "4"
 |x == "4" = "3"
 |x == "3" = "2"
 |x == "2" = "1"
 |x == "1" = "0"
 |otherwise = x

tiraNavio :: [String] -> [String] 
tiraNavio (x:xs) = (menosUm x) : xs

resultado :: Int -> [String] -> Char
resultado 1 (x:xs) = primChar x
resultado n (x:xs) = resultado (n-1) xs

ataque :: [String] -> [String] -> IO [String]
ataque a b = do
 coordenadas <- leCoordenada
 let tabuleiro = bomba (coordenadas) a b
 putStr(printTabuleiro 83 tabuleiro)
 if (resultado (coordenadas) tabuleiro) == 'A'
  then do
  	limpaTela
  	putStrLn "Você acertou a água!!\n"
  	return tabuleiro
  else do
  	limpaTela
  	putStrLn "Você acertou um navio!!\n"
  	let tabuleiro2 = tiraNavio tabuleiro
 	return tabuleiro2

-- Coloca B se acertar um navio ou A se acertar a água

bomba :: Int -> [String] -> [String] -> [String]
bomba n a [] = []
bomba n (x:xs) (y:ys)
 |(n == 1) && (x == "N") = "B" : ys 
 |(n == 1) && (x == " ") = "A" : ys
 |otherwise = y : bomba (n-1) xs ys

limpaTela :: IO ()
limpaTela = do
 putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

batalhaNaval = do
 limpaTela
 limpaTela
 p1 <- leNome1
 p2 <- leNome2
 let tabuleiro1 = colocaNome p1 tabuleiroP1
 let tabuleiro2 = colocaNome p2 tabuleiroP2
 let aux1 = colocaNome p1 auxP1
 let aux2 = colocaNome p2 auxP2
 limpaTela
 putStrLn "Vamos colocar os navios do primeiro jogador!!!\n"
 putStr(printTabuleiro 83 tabuleiro1)
 tabuleiroP1 <- colocaNavios tabuleiro1
 putStrLn "Pressione Enter para continuar."
 continuar <- getLine
 limpaTela
 putStrLn "Agora vamos colocar os navios do segundo jogador!!!\n"
 putStr(printTabuleiro 83 tabuleiro2)
 tabuleiroP2 <- colocaNavios tabuleiro2
 putStrLn "Pressione Enter para continuar."
 continuar <- getLine
 limpaTela
 ganhador <- comecaJogo 1 tabuleiroP1 aux1 tabuleiroP2 aux2
 let vencedor = if ganhador == 1
 	then p1
 	else p2
 putStrLn "Jogo finalizado!!!!\n\n\n"
 putStr("Vencedor: "++vencedor++"\n")
 limpaTela