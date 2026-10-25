module ModuloPar (esParBit) where

esParBit :: Int -> Bool
esParBit x = if (x `mod` 2) == 0 then True else False
