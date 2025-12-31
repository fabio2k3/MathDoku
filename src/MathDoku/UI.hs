{-# LANGUAGE RecordWildCards #-}
module MathDoku.UI (mainUI) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import MathDoku.Core
import MathDoku.Solver (resolverSudoku)
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
	{ board    :: Board
	, selected :: Maybe Coord
	, message  :: String
	}

initialState :: UIState
initialState = UIState
	{ board = emptyBoard
	, selected = Nothing
	, message = "Click para seleccionar, 1-9 para números, PLAY para resolver"
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
-- Drawing
-- =======================

drawUI :: UIState -> IO Picture
drawUI UIState{..} = return $
	pictures
		[ translate 0 gridYOffset $ drawGrid
		, translate 0 gridYOffset $ drawBoard board
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

drawBoard :: Board -> Picture
drawBoard board = pictures
	[ translate x y $
			scale 0.2 0.2 $
				color cellColor $
					text (show n)
	| i <- [0..8]
	, j <- [0..8]
	, let cell = getCell (i,j) board
	, Just n <- [cellToValue cell]
	, let x = -halfGrid + (fromIntegral j + 0.35) * cellSize
	, let y =  halfGrid - (fromIntegral i + 0.65) * cellSize
	, let cellColor = case cell of
						Fixed _ -> greyN 0.3
						_       -> black
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

drawResolveButton :: Picture
drawResolveButton =
	translate 0 (-halfGrid - 40) $
		pictures
			[ color (makeColorI 100 180 120 200) $
					rectangleSolid buttonWidth buttonHeight
			, color black $
					translate (-35) (-8) $
						scale 0.18 0.18 $
							text "RESOLVER"
			]

drawMessage :: String -> Picture
drawMessage "" = blank
drawMessage msg =
	translate 0 (-halfGrid - 100) $
		color (makeColorI 200 60 60 255) $
			scale 0.18 0.18 $
				text msg

-- =======================
-- Events
-- =======================

handleEvent :: Event -> UIState -> IO UIState
handleEvent ev s@UIState{..} = case ev of

	-- Click con el mouse
	EventKey (MouseButton LeftButton) Up _ (mx,my) ->
		case hitBoard mx (my - gridYOffset) of
			Just coord 
				| isCellEmpty coord board -> return s { selected = Just coord, message = "" }
				| otherwise -> return s { message = "Celda fija, elige otra" }
			Nothing
				| hitResolveButton mx my ->
						case resolverSudoku board of
							Just sol -> return s { board = sol, selected = Nothing, message = "¡Sudoku resuelto!" }
							Nothing  -> return s { message = "El Sudoku no tiene solución" }
				| otherwise -> return s

	-- Escribir números 1-9
	EventKey (Char c) Down _ _ ->
		case selected of
			Just coord@(i,j)
				| c >= '1' && c <= '9' ->
						let v = ord c - ord '0'
						in if isValidMove coord v board
							 then return s { board = placeValue coord v board
														 , selected = Nothing
														 , message = "" }
							 else return s { message = "Movimiento inválido" }
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

hitResolveButton :: Float -> Float -> Bool
hitResolveButton mx my =
	mx >= -buttonWidth/2 &&
	mx <=  buttonWidth/2 &&
	my >= (-halfGrid - 40 - buttonHeight/2) &&
	my <= (-halfGrid - 40 + buttonHeight/2)
