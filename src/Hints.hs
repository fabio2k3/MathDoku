-- Módulo Hints con funciones matemáticas reutilizables
module Hints
  ( esPar
  , esPrimo
  , esDivisible
  , enRango
  ) where

-- Función que verifica si un número es par o impar
esPar :: Int -> String
esPar n
    | even n    = "par"
    | otherwise = "impar"

-- Función que verifica si un número es primo
esPrimo :: Int -> String
esPrimo n
    | n < 2     = "no primo"
    | otherwise = if tieneDivisores n 2 then "no primo" else "primo"
    where
        tieneDivisores :: Int -> Int -> Bool
        tieneDivisores num divisor
            | divisor * divisor > num    = False
            | num `mod` divisor == 0     = True
            | otherwise                  = tieneDivisores num (divisor + 1)

-- Función que verifica si b es divisible por a
esDivisible :: Int -> Int -> String
esDivisible a b
    | a == 0    = "Error: no se puede dividir por cero"
    | b `mod` a == 0 = show b ++ " es divisible por " ++ show a
    | otherwise = show b ++ " no es divisible por " ++ show a

-- Función que verifica si un número está entre dos límites
enRango :: Int -> Int -> Int -> String
enRango limite1 limite2 numero =
    let (minimo, maximo) = if limite1 <= limite2 
                          then (limite1, limite2) 
                          else (limite2, limite1)
    in
        if numero >= minimo && numero <= maximo
            then show numero ++ " está entre " ++ show minimo ++ " y " ++ show maximo
            else show numero ++ " NO está entre " ++ show minimo ++ " y " ++ show maximo