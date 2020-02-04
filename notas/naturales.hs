-- Definición inductiva de los naturales
-- Cero es un número natural
-- si n es natural entonces n+1 también
-- :i Nat  chuleta para ver el tipo que es


data Nat = Cero | Suc Nat deriving Show
-- constructores
-- Cero :: Nat
-- Suc :: Nat -> Nat

uno :: Nat -- tipo de dato natural 
uno = Suc Cero
dos = Suc (Suc Cero)

tres = Suc dos

-- Este conjunto no son los naturales, TIENE MAYOR CARDINALIDAD
-- N \cup Nat( valores definidos)  and undefined not in 

indefinidoNat :: Nat
indefinidoNat = undefined

indefinido2Nat = Suc indefinidoNat
-- da más información indefinidoNat
-- valor del que no sabemos nada salvo que no es 0 


esCero :: Nat -> Bool
esCero Cero = True
esCero _ = False


-- prueba estas cosillas 
--esCero Cero
--esCero uno
--esCero indefinido2Nat 


-- infinito

infinitoN :: Nat
infinitoN = Suc infinitoN


esPar :: Nat -> Bool
esPar Cero = True
esPar (Suc n) = not (esPar n)


instance Eq Nat where
  Cero == Cero = True
  (Suc x) == (Suc y) = x == y
  _ == _ = False
  
instance Ord Nat where
  Cero <= _ = True
  (Suc x) <= (Suc y) = x <= y
  _ <= Cero = False 


