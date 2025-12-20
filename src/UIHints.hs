{-# LANGUAGE RecordWildCards #-}

module UIHints
  ( mainUIHints
  ) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Core
import Solve (resolverSudoku)
import Hints

-- =======================
-- Constantes UI
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

data HintType = HintParidad | HintPrimo
  deriving (Eq)

data UIState = UIState
  { userBoard :: Tablero
  , solution  :: Maybe Tablero
  , selected  :: Maybe Coord
  , hintMode  :: Maybe HintType
  , message   :: String
  }

initialState :: UIState
initialState = UIState
  { userBoard = crearTableroVacio
  , solution  = Nothing
  , selected  = Nothing
  , hintMode  = Nothing
  , message   = "Rellena el Sudoku y pulsa PLAY"
  }

-- =======================
-- Main
-- =======================

mainUIHints :: IO ()
mainUIHints = playIO
  (InWindow "MathDoku - Hints"
    (round (gridSize + 40), round (gridSize + 220))
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
    , drawNumbers userBoard
    , drawSelection selected
    , drawButtons
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

drawSelection :: Maybe Coord -> Picture
drawSelection Nothing = blank
drawSelection (Just (i,j)) =
  translate x y $
    color (makeColorI 200 220 255 120) $
      rectangleSolid (cellSize - 4) (cellSize - 4)
  where
    x = -halfGrid + (fromIntegral j + 0.5) * cellSize
    y =  halfGrid - (fromIntegral i + 0.5) * cellSize + gridYOffset

drawButtons :: Picture
drawButtons =
  pictures
    [ translate (-160) (-halfGrid - 40) $ button "PLAY"
    , translate ( 160) (-halfGrid - 40) $ button "CHECK"
    , translate (-100) (-halfGrid - 90) $ button "PAR"
    , translate ( 100) (-halfGrid - 90) $ button "PRIMO"
    ]

button :: String -> Picture
button label =
  pictures
    [ color (makeColorI 100 180 120 200) $
        rectangleSolid buttonWidth buttonHeight
    , color black $
        translate (-45) (-8) $
          scale 0.18 0.18 $
            text label
    ]

drawMessage :: String -> Picture
drawMessage "" = blank
drawMessage msg =
  translate 0 (-halfGrid - 140) $
    color (makeColorI 200 60 60 255) $
      scale 0.18 0.18 $
        text msg

-- =======================
-- Eventos
-- =======================

handleEvent :: Event -> UIState -> IO UIState
handleEvent ev s@UIState{..} = case ev of

  EventKey (MouseButton LeftButton) Up _ (mx,my) ->
    case hitBoard mx (my - gridYOffset) of
      Just coord ->
        case (hintMode, solution) of
          (Just h, Just sol) ->
            let (i,j) = coord
                v = sol !! i !! j
            in return s
                 { message = aplicarHint h v
                 , hintMode = Nothing
                 }
          _ ->
            return s { selected = Just coord }

      Nothing
        | hitPlay mx my ->
            case resolverSudoku userBoard of
              Just sol -> return s
                { solution = Just sol
                , message  = "Solución calculada"
                }
              Nothing  -> return s
                { message = "El Sudoku no tiene solución" }

        | hitCheck mx my ->
            case solution of
              Just sol ->
                return s
                  { message =
                      if userBoard == sol
                         then "✔ Sudoku correcto"
                         else "✘ Sudoku incorrecto"
                  }
              Nothing ->
                return s { message = "Pulsa PLAY primero" }

        | hitPar mx my ->
            return s { hintMode = Just HintParidad, message = "Elige una celda" }

        | hitPrimo mx my ->
            return s { hintMode = Just HintPrimo, message = "Elige una celda" }

        | otherwise -> return s

  EventKey (Char c) Down _ _ ->
    case selected of
      Just (i,j)
        | c >= '1' && c <= '9' ->
            return s
              { userBoard = colocarValor userBoard i j (read [c]) }
        | c == '0' ->
            return s
              { userBoard = colocarValor userBoard i j 0 }
      _ -> return s

  _ -> return s

-- =======================
-- Utilidades
-- =======================

aplicarHint :: HintType -> Int -> String
aplicarHint HintParidad = esPar
aplicarHint HintPrimo   = esPrimo

hitBoard :: Float -> Float -> Maybe Coord
hitBoard mx my
  | mx < -halfGrid || mx > halfGrid = Nothing
  | my < -halfGrid || my > halfGrid = Nothing
  | otherwise =
      let col = floor ((mx + halfGrid) / cellSize)
          row = floor ((halfGrid - my) / cellSize)
      in Just (row, col)

hitPlay, hitCheck, hitPar, hitPrimo :: Float -> Float -> Bool
hitPlay  = inButton (-160) (-halfGrid - 40)
hitCheck = inButton ( 160) (-halfGrid - 40)
hitPar   = inButton (-100) (-halfGrid - 90)
hitPrimo = inButton ( 100) (-halfGrid - 90)

inButton :: Float -> Float -> Float -> Float -> Bool
inButton cx cy mx my =
  mx >= cx - buttonWidth/2 &&
  mx <= cx + buttonWidth/2 &&
  my >= cy - buttonHeight/2 &&
  my <= cy + buttonHeight/2
