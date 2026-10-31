module Auxiliar where

-- Definiciones de las estructuras de datos
data Natural = Cero | S Natural deriving (Eq, Show)
data Ent = Zero | Succ Ent | Neg Ent deriving (Eq, Show)
data Matrioska = Mati | Cont Matrioska deriving (Eq, Show)

-- === Funciones Auxiliares para Naturales === 

-- Suma de naturales
sumaNat :: Natural -> Natural -> Natural
sumaNat Cero m = m
sumaNat (S n) m = S (sumaNat n m)

-- Multiplicación de naturales
multiNat :: Natural -> Natural -> Natural
multiNat Cero _ = Cero
multiNat (S n) m = sumaNat m (multiNat n m)
-- === Funciones Auxiliares para Enteros === --

-- Suma de enteros
suma_ent :: Ent -> Ent -> Ent
suma_ent Zero m = m
suma_ent n Zero = n
suma_ent (Succ n) (Succ m) = Succ (suma_ent n (Succ m))
suma_ent (Neg n) (Neg m) = Neg (suma_ent n m)
suma_ent (Succ n) (Neg m) = resta_ent (Succ n) m
suma_ent (Neg n) (Succ m) = resta_ent (Succ m) n

-- Resta de enteros
resta_ent :: Ent -> Ent -> Ent
resta_ent n Zero = n
resta_ent Zero (Succ m) = Neg (Succ m)
resta_ent Zero (Neg m) = m
resta_ent (Succ n) (Succ m) = resta_ent n m
resta_ent (Neg n) (Neg m) = resta_ent m n
resta_ent n (Neg m) = suma_ent n m
resta_ent (Neg n) (Succ m) = Neg (suma_ent n (Succ m))
