{- Descripcion : Para conversiones monetarias
 uso: El dinero lo hace  su valor decimal o dependendiendo .
-}
reconversion :: Double -> Double
reconversion x = x / 1000

{- Funcion cashback
	Descripcion :  calcula el %10  de una tarjeta
	uso: cashback 2545 -> 254 
-}

cashback:: Int -> Int
cashback x = x `div` 10

{-Funcion cashbackMonto
	descripcion : recibir dos decimales . dinero y el vuelto y lo va a devolver como decimales
	uso : cambia el valor an un cashback  cashbackMonto 294.4 = 29.4
-}
cashbackMonto :: Float -> Float -> Float
cashbackMonto dinero vuelto = dinero * vuelto


{-Descripcion : Reloj 
uso: un transformador de tiempo
-}
minutosHoras  :: Int -> String
minutosHoras m = show (m div 60) ++ " horas y " ++ show (m `mod`60)++ " minuto>

 {- descripcion: valúa si el cliente engaña al comerciante al no devolver el cam>
    uso:  no ser engañado, si no ser inteligente. 
-}
esEstafa :: Double -> Double -> Double -> Double -> Bool
esEstafa costo billeteGrande billeteChico cambioDevuelto =
    if (billeteChico + cambioDevuelto) /= billeteGrande
    then True
    else False
{- Descripcion: Verifica si cuatro números están ingresados en orden estrictamen>
 uso: esDescendente 10 9 8 7
  True
-}
esDescendente :: Double -> Double -> Double -> Double -> Bool  
esDescendente x y z w =
    if x > y && y > z && z > w
    then True
    else False
{-
Descripcion: Calcula el Índice de Masa Corporal y devuelve su clasificación segú>
uso: Para identifica el peso, uso para doctores o personas  que quieren saber
-}
imc :: Double -> Double -> String
imc kg estatura =
    if (kg / ((if estatura > 3.0 then estatura / 100.0 else estatura) * (if esta>
    then "bajo"
    else if (kg / ((if estatura > 3.0 then estatura / 100.0 else estatura) * (if>
         then "normal"
         else if (kg / ((if estatura > 3.0 then estatura / 100.0 else estatura) >
              then "sobrepeso"
              else "obesidad"
{-
Descripcion :Calcula la longitud de la hipotenusa de un triángulo rectángulo dad>
uso : determinar el largo de algún cable
-}
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (b * b + h * h)

{-
Descripcion :Calcula la inclinación o pendiente ($m$) de la recta que conecta do>
Uso : Un uso sería el de la rampa un plano.
-}
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)
{-
Descripcion: Calcula la distancia en la línea recta. 
uso: Es usado en coordenadas de GPS
-}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
