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
