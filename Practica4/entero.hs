import Auxiliar
{-5:
Uso: Toma dos enteros personalizados y regresa el producto de ellos.
-}
multiEnt :: Ent -> Ent -> Ent
multiEnt Zero _ = Zero
multiEnt _ Zero = Zero
-- Caso multiplicando dos positivos
multiEnt (Succ n) m = suma_ent m (multiEnt n m)

