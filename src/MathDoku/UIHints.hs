{-# LANGUAGE RecordWildCards #-}

module MathDoku.UIHints
  ( mainUIHints
  ) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import MathDoku.Core
import MathDoku.Solver (resolverSudoku)
import MathDoku.Hints (HintType(..), Hint(..), getHint, hintMessage, hintCost)
import Data.Char (ord)

-- =======================
-- UI Constants
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

-- =======================
-- State
-- =======================

data UIState = UIState
  { userBoard :: Board
  , solution  :: Maybe Board
  , selected  :: Maybe Coord
  , points    :: Int
  , message   :: String
  }

initialState :: UIState
initialState = UIState
  { userBoard = emptyBoard
  , solution  = Nothing
  , selected  = Nothing
  , points    = 10
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
-- Drawing
-- =======================

drawUI :: UIState -> IO Picture
drawUI UIState{..} = return $
  pictures
    [ translate 0 gridYOffset $ drawGrid
    , drawBoard userBoard
    , drawSelection selected
    , drawButtons points
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

drawBoard :: Board -> Picture
drawBoard board = pictures
  [ translate x y $
      scale 0.2 0.2 $
        color cellColor $
          text (show v)
  | i <- [0..8]
  , j <- [0..8]
  , let cell = getCell (i,j) board
  , let v = cellToValue cell
  , v /= Nothing
  , let x = -halfGrid + (fromIntegral j + 0.35) * cellSize
  , let y =  halfGrid - (fromIntegral i + 0.65) * cellSize + gridYOffset
  , let cellColor = case cell of 
                      Fixed _ -> greyN 0.3 
                      _ -> black
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

drawButtons :: Int -> Picture
drawButtons points =
  pictures
    [ translate (-160) (-halfGrid - 40) $ button "PLAY"
    , translate ( 160) (-halfGrid - 40) $ button "CHECK"
    , translate (-160) (-halfGrid - 90) $ button $ "PAR (" ++ show points ++ ")"
    , translate ( 160) (-halfGrid - 90) $ button "PRIMO"
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
-- Events
-- =======================

handleEvent :: Event -> UIState -> IO UIState
handleEvent ev s@UIState{..} = case ev of

  EventKey (MouseButton LeftButton) Up _ (mx,my) ->
    case hitBoard mx (my - gridYOffset) of
      Just coord ->
        case solution of
          Just sol ->
            case getHint Parity userBoard coord of
              Right hint ->
                let cost = hintCost (hintType hint)
                in if points >= cost
                   then return s 
                          { message = hintMessage hint
                          , points = points - cost
                          }
                   else return s { message = "Puntos insuficientes" }
              Left _ -> return s { message = "Celda inválida para pista" }
          _ -> return s { selected = Just coord }

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
            if isComplete userBoard && isValidBoard userBoard
              then return s { message = "✔ Sudoku correcto" }
              else return s { message = "✘ Sudoku incorrecto" }

        | otherwise -> return s

  EventKey (Char c) Down _ _ ->
    case selected of
      Just coord@(i,j)
        | c >= '1' && c <= '9' ->
            return s { userBoard = placeValue coord (ord c - ord '0') userBoard
                     , selected = Nothing }
      _ -> return s

  _ -> return s

-- =======================
-- Utilities
-- =======================

hitBoard :: Float -> Float -> Maybe Coord
hitBoard mx my
  | mx < -halfGrid || mx > halfGrid = Nothing
  | my < -halfGrid || my > halfGrid = Nothing
  | otherwise =
      let col = floor ((mx + halfGrid) / cellSize)
          row = floor ((halfGrid - my) / cellSize)
      in if row >= 0 && row <= 8 && col >= 0 && col <= 8
         then Just (row, col)
         else Nothing

hitPlay, hitCheck :: Float -> Float -> Bool
hitPlay  = inButton (-160) (-halfGrid - 40)
hitCheck = inButton ( 160) (-halfGrid - 40)

inButton :: Float -> Float -> Float -> Float -> Bool
inButton cx cy mx my =
  mx >= cx - buttonWidth/2 &&
  mx <= cx + buttonWidth/2 &&
  my >= cy - buttonHeight/2 &&
  my <= cy + buttonHeight/2
