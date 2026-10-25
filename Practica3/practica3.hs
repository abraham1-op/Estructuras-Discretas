import EsParBit (esParBit)
data Plazo = Dias Double | Meses Double
--Nuestra funcion auxiliar para  meses a días
tasaPorDias :: Double -> Double
tasaPorDias d = if d <= 28 then 0.0609
                else if d <= 182 then 0.0691
                else 0.0744

-- Nuestra funcion auxiliar para pasar meses a días:
-- regresa (días, tasa anual bruta) según el plazo.
tasaYDias :: Plazo -> (Double, Double)
tasaYDias plazo = case plazo of
    Dias 28   -> (28, 0.0609)
    Meses 1   -> (28, 0.0609)
    Dias 182  -> (182, 0.0691)
    Meses 6   -> (182, 0.0691)
    Dias 364  -> (364, 0.0744)
    Meses 12  -> (364, 0.0744)
    Dias d    -> (d, tasaPorDias d)
    Meses m   -> (m * 30, tasaPorDias (m * 30))
{-
Funcion 2. Cetes
Función que calcula el valor final de la inversión en Cetes aplicando la fórmula correspondiente con operaciones básicas.
-}

cetes :: Double -> Plazo -> Double
cetes vi plazo = 
    let (t, i) = tasaYDias plazo
    in vi * (1 + i * (t / 360))
-- 3. Función mensajeCetes

-- Función que genera un mensaje detallando el plazo, el rendimiento bruto y el monto total acumulado de la inversión.
mensajeCetes :: Double -> Plazo -> String
mensajeCetes vi plazo =
    let (t, i) = tasaYDias plazo
        vf = cetes vi plazo
        rendimiento = vf - vi
    in "Despues de " ++ show (round t :: Int) ++ " dias, obtuve " ++ show rendimiento ++ " de rendimiento bruto sin asumir ningun riesgo crediticio. El monto total de mi inversion es de " ++ show vf ++ "."
--4. Funcion negativos
{-
Cuenta la cantidad de números negativos en una lista
-}
negativos :: [Int] -> Int
negativos xs = length [x | x <- xs, x < 0]
--5 funcion soloPares
{-
Filtra usando nuestra funcion auxiliar esParbit.
-}
soloPares :: [Int] -> [Int]
soloPares xs = [x | x <- xs, esParBit x]
--6 Funcion primos
{-
Un número es primo si es mayor a 1 y sus únicos divisores exactos son 1 y él mismo
-}

esPrimo :: Int -> Bool
esPrimo n = if n <= 1 
            then False 
            else if length [x | x <- [1..n], n `mod` x == 0] == 2 
                 then True 
                 else False

primos :: [Int] -> [Int]
primos xs = [x | x <- xs, esPrimo x]
-- 7. Función coprimos
-- lista de los numeros entre 1 y n que son coprimos con n.
--Se uso un auxiliar de mcd para practicidar
mcd :: Int -> Int -> Int
mcd a b = if b == 0  
          then a  
          else mcd b (a `mod` b)
coprimos :: Int -> [Int]
coprimos n = [x | x <- [1..n], mcd n x == 1]
