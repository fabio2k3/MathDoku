{-# LANGUAGE FlexibleContexts #-}

module Solve
  ( Tablero
  , resolverSudoku
  , imprimirTablero
  , crearTableroVacio
  ) where

import Data.List (transpose, nub, (\\))
import Control.Monad (forM_, when)

-- Tipo para representar el tablero de Sudoku
type Tablero = [[Int]]

-- Crear un tablero vacío
crearTableroVacio :: Tablero
crearTableroVacio = replicate 9 (replicate 9 0)

-- Obtener una fila específica
obtenerFila :: Tablero -> Int -> [Int]
obtenerFila tablero i = tablero !! i

-- Obtener una columna específica
obtenerColumna :: Tablero -> Int -> [Int]
obtenerColumna tablero j = map (!! j) tablero

-- Obtener una región 3x3 específica
obtenerRegion :: Tablero -> Int -> Int -> [Int]
obtenerRegion tablero i j =
  [ tablero !! r !! c
  | r <- [3*i .. 3*i+2]
  , c <- [3*j .. 3*j+2]
  ]

-- Encontrar la siguiente celda vacía (0)
encontrarCeldaVacia :: Tablero -> Maybe (Int, Int)
encontrarCeldaVacia tablero =
  case [(i, j) | i <- [0..8], j <- [0..8], tablero !! i !! j == 0] of
    [] -> Nothing
    (x:_) -> Just x

-- Posibles valores válidos para la celda (i,j)
obtenerPosibles :: Tablero -> Int -> Int -> [Int]
obtenerPosibles tablero i j =
  let fila = obtenerFila tablero i
      columna = obtenerColumna tablero j
      region = obtenerRegion tablero (i `div` 3) (j `div` 3)
      usados = nub $ filter (/= 0) (fila ++ columna ++ region)
  in [1..9] \\ usados

-- Resolver el Sudoku (función principal)
resolverSudoku :: Tablero -> Maybe Tablero
resolverSudoku tablero =
  case encontrarCeldaVacia tablero of
    Nothing -> Just tablero
    Just (i, j) ->
      let posibles = obtenerPosibles tablero i j
      in try posibles i j
  where
    try [] _ _ = Nothing
    try (n:ns) i j =
      let nuevoTablero = actualizarTablero tablero i j n
      in case resolverSudoku nuevoTablero of
           Just solucion -> Just solucion
           Nothing -> try ns i j

-- Actualizar una celda en el tablero
actualizarTablero :: Tablero -> Int -> Int -> Int -> Tablero
actualizarTablero tablero i j valor =
  let fila = tablero !! i
      filaMod = take j fila ++ [valor] ++ drop (j + 1) fila
  in take i tablero ++ [filaMod] ++ drop (i + 1) tablero

-- Imprimir el tablero de forma legible
imprimirTablero :: Tablero -> IO ()
imprimirTablero tablero = do
  putStrLn "┌─────────┬─────────┬─────────┐"
  forM_ [0..8] $ \i -> do
    putStr "│"
    forM_ [0..8] $ \j -> do
      let valor = tablero !! i !! j
      putStr $ if valor == 0 then " ." else " " ++ show valor
      when (j `mod` 3 == 2) $ putStr " │"
    putStrLn ""
    when (i `mod` 3 == 2 && i < 8) $
      putStrLn "├─────────┼─────────┼─────────┤"
  putStrLn "└─────────┴─────────┴─────────┘"