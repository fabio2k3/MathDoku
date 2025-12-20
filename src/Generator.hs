{-# LANGUAGE FlexibleContexts #-}

module Generator
  ( generarSudoku
  , generarTableroCompleto
  , removerCeldas
  , Dificultad(..)
  ) where

import Core
import Solve (resolverSudoku)
import System.Random (randomRIO, StdGen, mkStdGen, randomR)
import Data.List (permutations, (\\))
import Control.Monad (foldM)

-- Niveles de dificultad
data Dificultad = Facil | Medio | Dificil | Experto
  deriving (Show, Eq)

-- Obtener número de celdas a remover según dificultad
celdasARemover :: Dificultad -> Int
celdasARemover Facil = 30     -- ~37% vacío
celdasARemover Medio = 40     -- ~49% vacío
celdasARemover Dificil = 50   -- ~62% vacío
celdasARemover Experto = 60   -- ~74% vacío

-- Generar un Sudoku completo aleatorio
generarTableroCompleto :: IO Tablero
generarTableroCompleto = do
  -- Empezar con un tablero vacío
  let tableroBase = crearTableroVacio
  -- Llenar la primera fila con números aleatorios
  primeraFila <- shuffle [1..9]
  let tableroConFila = colocarFila tableroBase 0 primeraFila
  -- Resolver el tablero para obtener una solución completa válida
  case resolverSudoku tableroConFila of
    Just tableroCompleto -> return tableroCompleto
    Nothing -> generarTableroCompleto  -- Reintentar si falla

-- Colocar una fila completa en el tablero
colocarFila :: Tablero -> Int -> [Int] -> Tablero
colocarFila tablero _ [] = tablero
colocarFila tablero fila valores =
  foldl (\t (col, val) -> colocarValor t fila col val) tablero (zip [0..] valores)

-- Mezclar una lista aleatoriamente (Fisher-Yates shuffle)
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  indices <- mapM (\i -> randomRIO (0, i)) [0..length xs - 1]
  return $ shuffleWith indices xs
  where
    shuffleWith [] _ = []
    shuffleWith (i:is) ys = 
      let (before, x:after) = splitAt i ys
      in x : shuffleWith is (before ++ after)

-- Remover celdas del tablero manteniendo solución única
removerCeldas :: Tablero -> Int -> IO Tablero
removerCeldas tablero 0 = return tablero
removerCeldas tablero n = do
  -- Obtener lista de celdas no vacías
  let celdasLlenas = [(i, j) | i <- [0..8], j <- [0..8], 
                                obtenerValor tablero i j /= 0]
  
  if null celdasLlenas
    then return tablero
    else do
      -- Elegir una celda aleatoria
      idx <- randomRIO (0, length celdasLlenas - 1)
      let (i, j) = celdasLlenas !! idx
      let valorOriginal = obtenerValor tablero i j
      
      -- Intentar remover la celda
      let tableroModificado = colocarValor tablero i j 0
      
      -- Verificar si sigue teniendo solución única
      if tieneSolucionUnica tableroModificado
        then removerCeldas tableroModificado (n - 1)
        else removerCeldas tablero (n - 1)  -- No remover, intentar otra celda

-- Verificar si el tablero tiene solución única
-- (Simplificado: solo verificamos que tenga al menos una solución)
-- Para una implementación completa, habría que contar todas las soluciones
tieneSolucionUnica :: Tablero -> Bool
tieneSolucionUnica tablero =
  case resolverSudoku tablero of
    Just _ -> True   -- Tiene solución (asumir única por ahora)
    Nothing -> False -- No tiene solución

-- Función principal: generar un Sudoku con dificultad especificada
generarSudoku :: Dificultad -> IO Tablero
generarSudoku dificultad = do
  tableroCompleto <- generarTableroCompleto
  let numCeldas = celdasARemover dificultad
  removerCeldas tableroCompleto numCeldas

-- Imprimir tablero para debug
imprimirTablero :: Tablero -> IO ()
imprimirTablero tablero = do
  mapM_ print tablero
