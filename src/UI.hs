{-# LANGUAGE RecordWildCards #-}
module UI
  ( mainUI
  ) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Core
  ( Tablero
  , crearTableroVacio
  , colocarValor
  , esMovimientoValido
  )

import Solve (resolverSudoku)

-- =======================
-- Constantes de UI
-- =======================

cellSize :: Float
cellSize = 50

gridSize :: Float
gridSize = 9 * cellSize

halfGrid :: Float
halfGrid = gridSize / 2

gridYOffset :: Float
gridYOffset = 40

buttonWidth, buttonHeight :: Float
buttonWidth = 140
buttonHeight = 40

type Coord = (Int, Int)

-- =======================
-- Estado
-- =======================

data UIState = UIState
  { board    :: Tablero
  , selected :: Maybe Coord
  , message  :: String
  }

initialState :: UIState
initialState = UIState
  { board = crearTableroVacio
  , selected = Just (0,0)
  , message = ""
  }

-- =======================
-- Main
-- =======================

mainUI :: IO ()
mainUI = playIO
  (InWindow "MathDoku - Sudoku"
            (round (gridSize + 40), round (gridSize + 160))
            (100,100))
  white
  30
  initialState
  drawUI
  handleEvent
  (\_ s -> return s)

-- =======================
-- Dibujo
-- =======================

drawUI :: UIState -> IO Picture
drawUI UIState{..} = return $
  pictures
    [ translate 0 gridYOffset drawGrid
    , drawNumbers board
    , drawSelection selected
    , drawResolveButton
    , drawMessage message
    ]

drawGrid :: Picture
drawGrid = color black $ pictures $
  [ line [(-halfGrid,  halfGrid - i*cellSize)
         ,( halfGrid,  halfGrid - i*cellSize)]
  | i <- [0..9]
  ] ++
  [ line [(-halfGrid + j*cellSize,  halfGrid)
         ,(-halfGrid + j*cellSize, -halfGrid)]
  | j <- [0..9]
  ]

-- =======================
-- Números centrados
-- =======================

drawNumbers :: Tablero -> Picture
drawNumbers tab = pictures
  [ translate x y $
      scale 0.2 0.2 $
        text (show v)
  | i <- [0..8]
  , j <- [0..8]
  , let v = tab !! i !! j
  , v /= 0
  , let x = -halfGrid + (fromIntegral j + 0.35) * cellSize
  , let y =  halfGrid - (fromIntegral i + 0.65) * cellSize + gridYOffset
  ]

-- =======================
-- Selección
-- =======================

drawSelection :: Maybe Coord -> Picture
drawSelection Nothing = blank
drawSelection (Just (i,j)) =
  translate x y $
    color (makeColorI 200 220 255 120) $
      rectangleSolid (cellSize - 4) (cellSize - 4)
  where
    x = -halfGrid + (fromIntegral j + 0.5) * cellSize
    y =  halfGrid - (fromIntegral i + 0.5) * cellSize + gridYOffset

-- =======================
-- Botón Resolve
-- =======================

drawResolveButton :: Picture
drawResolveButton =
  translate 0 (-halfGrid - 40) $
    pictures
      [ color (makeColorI 100 180 120 200) $
          rectangleSolid buttonWidth buttonHeight
      , color black $
          translate (-45) (-8) $
            scale 0.18 0.18 $
              text "Resolve"
      ]

-- =======================
-- Mensajes
-- =======================

drawMessage :: String -> Picture
drawMessage "" = blank
drawMessage msg =
  translate 0 (-halfGrid - 100) $
    color (makeColorI 200 60 60 255) $
      scale 0.18 0.18 $
        text msg

-- =======================
-- Eventos
-- =======================

handleEvent :: Event -> UIState -> IO UIState
handleEvent ev s@UIState{..} = case ev of

  -- Click con el mouse
  EventKey (MouseButton LeftButton) Up _ (mx,my) ->
    case hitBoard mx (my - gridYOffset) of
      Just coord -> return s { selected = Just coord, message = "" }
      Nothing ->
        if hitResolveButton mx my
           then case resolverSudoku board of
                  Just sol -> return s { board = sol, selected = Nothing, message = "" }
                  Nothing  -> return s { message = "NO TIENE SOLUCION" }
           else return s

  -- Escribir números
  EventKey (Char c) Down _ _ ->
    case selected of
      Just (i,j)
        | c >= '1' && c <= '9' ->
            let v = read [c]
            in if esMovimientoValido board i j v
                  then return s { board = colocarValor board i j v, message = "" }
                  else return s { message = "Movimiento inválido" }
        | c == '0' ->
            return s { board = colocarValor board i j 0, message = "" }
      _ -> return s

  -- Flechas + borrar
  EventKey (SpecialKey key) Down _ _ ->
    return $ case key of
      KeyUp        -> s { selected = move (-1)  0 selected }
      KeyDown      -> s { selected = move   1   0 selected }
      KeyLeft      -> s { selected = move   0 (-1) selected }
      KeyRight     -> s { selected = move   0   1 selected }
      KeyBackspace -> case selected of
                        Just (i,j) -> s { board = colocarValor board i j 0 }
                        Nothing    -> s
      _ -> s

  _ -> return s

-- =======================
-- Utilidades
-- =======================

move :: Int -> Int -> Maybe Coord -> Maybe Coord
move _ _ Nothing = Nothing
move di dj (Just (i,j)) =
  Just ( clamp (i+di), clamp (j+dj) )

clamp :: Int -> Int
clamp = max 0 . min 8

hitBoard :: Float -> Float -> Maybe Coord
hitBoard mx my
  | mx < -halfGrid || mx > halfGrid = Nothing
  | my < -halfGrid || my > halfGrid = Nothing
  | otherwise =
      let col = floor ((mx + halfGrid) / cellSize)
          row = floor ((halfGrid - my) / cellSize)
      in Just (row, col)

hitResolveButton :: Float -> Float -> Bool
hitResolveButton mx my =
  mx >= -buttonWidth/2 &&
  mx <=  buttonWidth/2 &&
  my >= (-halfGrid - 40 - buttonHeight/2) &&
  my <= (-halfGrid - 40 + buttonHeight/2)
