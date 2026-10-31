module Natural where
import Auxiliar
{-
  Función: a_natural
  Como en ejemplo que nos puso Irving 
  Descripción: Convierte un entero nativo de Haskell (0,1,2...) a su estructura Natural.
  Ejemplos de uso:
  ghci> a_natural 0 
  Cero
  ghci> a_natural 2
  S (S Cero)
-}

a_natural :: Int -> Natural
a_natural n
 |  n <= 0    = Cero
 |  True = S (a_natural (n - 1))


