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
