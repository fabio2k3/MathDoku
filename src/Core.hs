module Core
  ( Tablero
  , Celda
  , crearTableroVacio
  , esCeldaVacia
  , obtenerValor
  , colocarValor
  , esMovimientoValido
  , obtenerFila
  , obtenerColumna
  , obtenerRegion
  , tableroCompleto
  , tableroValido
  ) where

import Data.List (nub)

-- Tipos básicos
type Tablero = [[Int]]
type Celda = (Int, Int)  -- (fila, columna)

-- Crear un tablero vacío (9x9 con ceros)
crearTableroVacio :: Tablero
crearTableroVacio = replicate 9 (replicate 9 0)

-- Verificar si una celda está vacía
esCeldaVacia :: Tablero -> Int -> Int -> Bool
esCeldaVacia tablero i j = tablero !! i !! j == 0

-- Obtener el valor de una celda
obtenerValor :: Tablero -> Int -> Int -> Int
obtenerValor tablero i j = tablero !! i !! j

-- Colocar un valor en una celda
colocarValor :: Tablero -> Int -> Int -> Int -> Tablero
colocarValor tablero i j valor =
  let fila = tablero !! i
      nuevaFila = take j fila ++ [valor] ++ drop (j + 1) fila
  in take i tablero ++ [nuevaFila] ++ drop (i + 1) tablero

-- Obtener una fila específica
obtenerFila :: Tablero -> Int -> [Int]
obtenerFila tablero i = tablero !! i

-- Obtener una columna específica
obtenerColumna :: Tablero -> Int -> [Int]
obtenerColumna tablero j = map (!! j) tablero

-- Obtener una región 3x3 específica
obtenerRegion :: Tablero -> Int -> Int -> [Int]
obtenerRegion tablero i j =
  let regionI = (i `div` 3) * 3
      regionJ = (j `div` 3) * 3
  in [ tablero !! r !! c
     | r <- [regionI .. regionI + 2]
     , c <- [regionJ .. regionJ + 2]
     ]

-- Verificar si un movimiento es válido
esMovimientoValido :: Tablero -> Int -> Int -> Int -> Bool
esMovimientoValido tablero i j valor
  | valor < 1 || valor > 9 = False
  | otherwise =
      let fila = obtenerFila tablero i
          columna = obtenerColumna tablero j
          region = obtenerRegion tablero i j
          -- Filtrar ceros y verificar que el valor no exista
      in valor `notElem` filter (/= 0) fila &&
         valor `notElem` filter (/= 0) columna &&
         valor `notElem` filter (/= 0) region

-- Verificar si el tablero está completo (sin ceros)
tableroCompleto :: Tablero -> Bool
tableroCompleto tablero = all (all (/= 0)) tablero

-- Verificar si el tablero es válido (sin repeticiones)
tableroValido :: Tablero -> Bool
tableroValido tablero =
  todasFilasValidas && todasColumnasValidas && todasRegionesValidas
  where
    todasFilasValidas = all esListaValida tablero
    todasColumnasValidas = all esListaValida [obtenerColumna tablero j | j <- [0..8]]
    todasRegionesValidas = all esListaValida 
      [obtenerRegion tablero (3*i) (3*j) | i <- [0..2], j <- [0..2]]
    
    -- Una lista es válida si no tiene duplicados (excluyendo ceros)
    esListaValida xs = 
      let sinCeros = filter (/= 0) xs
      in sinCeros == nub sinCeros
