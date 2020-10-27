-- Programação Funcional - Lista 11

-- Moniele K. Santos

--numero 1
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
 deriving(Eq, Show)

--numero 2
finalDeSemana :: Dia -> Bool
finalDeSemana (s)
 | s == Sabado = True
 | s == Domingo = True
 | otherwise = False

--numero 3

data TalvezFloat = Valor Float | Erro String
 deriving(Eq, Show)

--numero 4

divisao :: Float -> Float -> TalvezFloat

divisao n1 n2
 | n2 == 0 = Erro "divisao por zero"
 | otherwise = Valor (n1/n2)

 --numero 5


data Nat = Zero | Suc Nat
 deriving(Eq,Show)

dois :: Nat 
dois = Suc(Suc Zero)

sete :: Nat
sete = Suc(Suc(Suc(Suc(Suc(Suc(Suc Zero))))))



natToInt :: Nat -> Int


natToInt Zero = 0
natToInt (Suc nat) = 1 + (natToInt nat)

--obs: o compilador de haskell n permite dois tipos diferentes de argumentos, por isso usei o () para q seja 'convertido' 
--para apenas 1 parametro em (Suc nat)

--numero 6

intToNat :: Int -> Nat

intToNat 1 = Suc Zero
intToNat n = Suc(intToNat (n-1))

