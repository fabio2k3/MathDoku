{-# LANGUAGE RecordWildCards #-}

module MathDoku.UIHints
  ( mainUIHints
  ) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import MathDoku.Core
import MathDoku.Solver (resolverSudoku)
import MathDoku.Generator (generatePuzzle)
import MathDoku.GameState
import MathDoku.Hints
import Data.Char (ord, isDigit)
import Data.Time (getCurrentTime, NominalDiffTime)

-- ============================================================================
-- UI CONSTANTS
-- ============================================================================

cellSize, gridSize, halfGrid, gridYOffset :: Float
cellSize = 50
gridSize = 9 * cellSize
halfGrid = gridSize / 2
gridYOffset = 60

buttonWidth, buttonHeight :: Float
buttonWidth = 120
buttonHeight = 40

hintButtonWidth :: Float
hintButtonWidth = 100

-- ============================================================================
-- MAIN ENTRY POINT
-- ============================================================================

mainUIHints :: IO ()
mainUIHints = playIO
  (InWindow "MathDoku - Mathematical Sudoku"
    (round (gridSize + 80), round (gridSize + 400))
    (100, 50))
  white
  30
  initialState
  drawUI
  handleEvent
  (\_ s -> return s)

-- ============================================================================
-- DRAWING FUNCTIONS
-- ============================================================================

drawUI :: GameState -> IO Picture
drawUI state@GameState{..} = return $ case gsPhase of
  SelectDifficulty -> drawMenuScreen state
  GeneratingPuzzle _ -> drawLoadingScreen
  Playing -> drawGameScreen state
  GameWon elapsed -> drawVictoryScreen state elapsed

-- ----------------------------------------------------------------------------
-- MENU SCREEN
-- ----------------------------------------------------------------------------

drawMenuScreen :: GameState -> Picture
drawMenuScreen GameState{..} = pictures
  [ -- Title
    translate 0 150 $ scale 0.4 0.4 $ color (makeColorI 50 100 200 255) $
      text "MATHDOKU"
  , translate 0 110 $ scale 0.15 0.15 $ color black $
      text "Mathematical Sudoku with Smart Hints"
  
    -- Difficulty buttons
  , translate 0 30 $ scale 0.2 0.2 $ color black $ text "Select Difficulty:"
  , translate (-150) (-30) $ drawDiffButton Easy (gsSelectedDiff == Easy)
  , translate 0 (-30) $ drawDiffButton Medium (gsSelectedDiff == Medium)
  , translate 150 (-30) $ drawDiffButton Hard (gsSelectedDiff == Hard)
  , translate 0 (-90) $ drawDiffButton Expert (gsSelectedDiff == Expert)
  
    -- Info text
  , translate 0 (-150) $ scale 0.12 0.12 $ color (greyN 0.3) $
      text $ difficultyInfo gsSelectedDiff
  
    -- START button
  , translate 0 (-210) $ drawButton "START GAME" (makeColorI 100 180 120 255) True
  
    -- Message
  , translate 0 (-270) $ scale 0.15 0.15 $ color (makeColorI 200 60 60 255) $
      text gsMessage
  ]

drawDiffButton :: Difficulty -> Bool -> Picture
drawDiffButton diff selected = pictures
  [ color fillColor $ rectangleSolid 130 35
  , color black $ rectangleWire 130 35
  , translate (-40) (-5) $ scale 0.15 0.15 $ color black $ text label
  ]
  where
    label = show diff
    fillColor = if selected 
                then makeColorI 100 200 255 255  -- Light blue
                else makeColorI 220 220 220 255  -- Light grey

difficultyInfo :: Difficulty -> String
difficultyInfo Easy   = "Easy: 40 cells to fill, 15 points for hints"
difficultyInfo Medium = "Medium: 50 cells, 12 points"
difficultyInfo Hard   = "Hard: 60 cells, 10 points"
difficultyInfo Expert = "Expert: 77 cells, 8 points - Good luck!"

-- ----------------------------------------------------------------------------
-- LOADING SCREEN
-- ----------------------------------------------------------------------------

drawLoadingScreen :: Picture
drawLoadingScreen = pictures
  [ translate 0 0 $ scale 0.3 0.3 $ color black $ text "Generating puzzle..."
  , translate 0 (-50) $ scale 0.15 0.15 $ color (greyN 0.5) $ 
      text "This may take a moment for Expert difficulty"
  ]

-- ----------------------------------------------------------------------------
-- GAME SCREEN
-- ----------------------------------------------------------------------------

drawGameScreen :: GameState -> Picture
drawGameScreen state@GameState{..} = pictures
  [ -- Grid and board
    translate 0 gridYOffset drawGrid
  , drawBoard gsBoard gsOriginal
  , drawSelection gsSelectedCell
  
    -- Points display
  , translate 0 (gridYOffset + halfGrid + 30) $ drawPointsBar gsPoints gsInitialPoints
  
    -- Hint buttons (left side)
  , translate (-gridSize/2 - 70) 200 $ drawHintButtons state
  
    -- Control buttons 
  , translate 0 (-halfGrid - 20) $ drawControlButtons
  
    -- Message bar (más arriba)
  , translate 0 (-halfGrid - 120) $ drawMessage gsMessage
  
    -- Stats (right side)
  , translate (gridSize/2 + 70) 150 $ drawStats gsStats gsMovesCount
  ]

-- Draw Sudoku grid
drawGrid :: Picture
drawGrid = color black $ pictures $
  -- Horizontal lines
  [ line [(-halfGrid, halfGrid - i*cellSize), (halfGrid, halfGrid - i*cellSize)]
  | i <- [0..9]
  ] ++
  -- Vertical lines
  [ line [(-halfGrid + j*cellSize, halfGrid), (-halfGrid + j*cellSize, -halfGrid)]
  | j <- [0..9]
  ] ++
  -- Thick lines for 3x3 boxes
  [ color (makeColorI 0 0 0 255) $ lineLoop
    [ (-halfGrid + 3*i*cellSize, halfGrid)
    , (-halfGrid + 3*i*cellSize, -halfGrid)
    ]
  | i <- [0..3]
  ] ++
  [ color (makeColorI 0 0 0 255) $ lineLoop
    [ (-halfGrid, halfGrid - 3*i*cellSize)
    , (halfGrid, halfGrid - 3*i*cellSize)
    ]
  | i <- [0..3]
  ]

-- Draw board numbers
drawBoard :: Board -> Board -> Picture
drawBoard current original = pictures
  [ translate x y $
      scale 0.25 0.25 $
        color cellColor $
          text (show n)
  | i <- [0..8]
  , j <- [0..8]
  , let cell = getCell (i,j) current
        origCell = getCell (i,j) original
  , Just n <- [cellToValue cell]
  , let x = -halfGrid + (fromIntegral j + 0.3) * cellSize
  , let y = halfGrid - (fromIntegral i + 0.7) * cellSize + gridYOffset
  , let cellColor = case origCell of
                      Empty -> makeColorI 0 150 0 255    -- User cells = green
                      _ -> makeColorI 50 50 50 255      -- Original = dark grey
  ]

-- Draw selection highlight
drawSelection :: Maybe Coord -> Picture
drawSelection Nothing = blank
drawSelection (Just (i,j)) =
  translate x y $
    color (makeColorI 255 255 0 100) $  -- Yellow highlight
      rectangleSolid (cellSize - 4) (cellSize - 4)
  where
    x = -halfGrid + (fromIntegral j + 0.5) * cellSize
    y = halfGrid - (fromIntegral i + 0.5) * cellSize + gridYOffset

-- Draw points bar
drawPointsBar :: Int -> Int -> Picture
drawPointsBar current total = pictures
  [ -- Label
    translate (-150) 0 $ scale 0.2 0.2 $ color black $ text "Points:"
  , -- Bar background
    translate 50 0 $ color (greyN 0.9) $ rectangleSolid 200 20
  , -- Bar fill
    let percentage = fromIntegral current / fromIntegral total
        barWidth = 200 * percentage
    in translate (50 - (200 - barWidth)/2) 0 $
         color (makeColorI 100 200 100 255) $ rectangleSolid barWidth 20
  , -- Number
    translate 170 (-5) $ scale 0.15 0.15 $ color black $
      text $ show current ++ "/" ++ show total
  ]

-- Draw hint buttons (vertical column)
drawHintButtons :: GameState -> Picture
drawHintButtons GameState{..} = pictures
  [ translate 0 0 $ scale 0.15 0.15 $ color black $ text "HINTS:"
  , translate 0 (-40) $ drawHintButton Parity gsPoints (gsSelectedHint == Parity)
  , translate 0 (-90) $ drawHintButton Prime gsPoints (gsSelectedHint == Prime)
  , translate 0 (-140) $ drawHintButton Divisible3 gsPoints (gsSelectedHint == Divisible3)
  , translate 0 (-190) $ drawHintButton NarrowRange gsPoints (gsSelectedHint == NarrowRange)
  , translate 0 (-240) $ drawHintButton Reveal gsPoints (gsSelectedHint == Reveal)
  ]

drawHintButton :: HintType -> Int -> Bool -> Picture
drawHintButton hType availPoints selected = pictures
  [ -- Background
    color fillColor $ rectangleSolid hintButtonWidth 35
  , color black $ rectangleWire hintButtonWidth 35
  , -- Label
    translate (-45) 5 $ scale 0.12 0.12 $ color textColor $ text label
  , -- Cost
    translate (-45) (-10) $ scale 0.1 0.1 $ color textColor $
      text $ "(" ++ show (hintCost hType) ++ "pts)"
  ]
  where
    canAfford = availPoints >= hintCost hType
    label = case hType of
      Parity -> "PARITY"
      Prime -> "PRIME"
      Divisible3 -> "DIV by 3"
      NarrowRange -> "RANGE ±1"
      Reveal -> "REVEAL"
      _ -> show hType
    
    fillColor
      | selected && canAfford = makeColorI 100 200 255 255  -- Blue (selected, affordable)
      | selected = makeColorI 255 150 150 255               -- Red (selected, can't afford)
      | canAfford = makeColorI 200 255 200 255              -- Light green (affordable)
      | otherwise = makeColorI 200 200 200 255              -- Grey (can't afford)
    
    textColor = if canAfford then black else greyN 0.6

-- Draw control buttons
drawControlButtons :: Picture
drawControlButtons = pictures
  [ translate (-130) 0 $ drawButton "CHECK" (makeColorI 100 150 255 255) True
  , translate 0 0 $ drawButton "SOLVE" (makeColorI 255 150 100 255) True
  , translate 130 0 $ drawButton "RESET" (makeColorI 200 100 100 255) True
  ]

-- Generic button
drawButton :: String -> Color -> Bool -> Picture
drawButton label bgColor enabled = pictures
  [ color bgColor $ rectangleSolid buttonWidth buttonHeight
  , color black $ rectangleWire buttonWidth buttonHeight
  , translate (-buttonWidth/2 + 10) (-10) $ scale 0.11 0.11 $ 
      color (if enabled then black else greyN 0.5) $ text label
  ]

drawWideButton :: String -> Color -> Bool -> Picture
drawWideButton label bgColor enabled = pictures
  [ color bgColor $ rectangleSolid (buttonWidth+60) buttonHeight
  , color black $ rectangleWire (buttonWidth+60) buttonHeight
  , translate (-(buttonWidth+60)/2 + 10) (-10) $ scale 0.10 0.10 $ 
      color (if enabled then black else greyN 0.5) $ text label
  ]

-- Draw stats panel
drawStats :: Statistics -> Int -> Picture
drawStats Statistics{..} moves = pictures
  [ translate 0 0 $ scale 0.15 0.15 $ color black $ text "STATS:"
  , translate 0 (-30) $ statLine "Moves:" (show moves)
  , translate 0 (-55) $ statLine "Filled:" (show statCellsFilled)
  , translate 0 (-80) $ statLine "Errors:" (show statErrors)
  , translate 0 (-105) $ statLine "Hints:" (show statHintsUsed)
  ]

statLine :: String -> String -> Picture
statLine label value = pictures
  [ translate 0 0 $ scale 0.12 0.12 $ color (greyN 0.4) $ text label
  , translate 120 0 $ scale 0.12 0.12 $ color black $ text value
  ]

-- Draw message
drawMessage :: String -> Picture
drawMessage "" = blank
drawMessage msg = pictures
  [ color (makeColorI 255 255 200 255) $ rectangleSolid 600 50
  , translate (-290) (-15) $ scale 0.18 0.18 $ color (makeColorI 150 0 0 255) $
      text msg
  ]

-- ----------------------------------------------------------------------------
-- VICTORY SCREEN
-- ----------------------------------------------------------------------------

drawVictoryScreen :: GameState -> NominalDiffTime -> Picture
drawVictoryScreen GameState{..} elapsed = pictures
  [ -- Title
    translate 0 180 $ scale 0.5 0.5 $ color (makeColorI 255 215 0 255) $ text "VICTORY!"
    -- Stats
  , translate 0 100 $ scale 0.18 0.18 $ color black $ text $ "Time: " ++ formatTime elapsed
  , translate 0 60 $ scale 0.18 0.18 $ color black $ text $ "Final Score: " ++ show (getCurrentScore GameState{..})
  , translate 0 0 $ drawDetailedStats gsStats
    -- Botón más ancho y texto más pequeño
  , translate 0 (-180) $ drawWideButton "PLAY AGAIN" (makeColorI 100 180 120 255) True
  ]

drawDetailedStats :: Statistics -> Picture
drawDetailedStats Statistics{..} = pictures
  [ translate 0 20 $ scale 0.15 0.15 $ color black $ text "Game Statistics:"
  , translate 0 (-10) $ statLine "Cells Filled:" (show statCellsFilled)
  , translate 0 (-35) $ statLine "Errors Made:" (show statErrors)
  , translate 0 (-60) $ statLine "Hints Used:" (show statHintsUsed)
  , translate 0 (-85) $ statLine "  - Parity:" (show statParityUsed)
  , translate 0 (-105) $ statLine "  - Prime:" (show statPrimeUsed)
  , translate 0 (-125) $ statLine "  - Range:" (show statRangeUsed)
  , translate 0 (-145) $ statLine "  - Reveal:" (show statRevealUsed)
  ]

formatTime :: NominalDiffTime -> String
formatTime t =
  let totalSeconds = floor t :: Int
      minutes = totalSeconds `div` 60
      seconds = totalSeconds `mod` 60
  in show minutes ++ "m " ++ show seconds ++ "s"

-- ============================================================================
-- EVENT HANDLING
-- ============================================================================

handleEvent :: Event -> GameState -> IO GameState
handleEvent event state = case gsPhase state of
  SelectDifficulty -> handleMenuEvent event state
  Playing -> handleGameEvent event state
  GameWon _ -> handleVictoryEvent event state
  _ -> return state

-- ----------------------------------------------------------------------------
-- MENU EVENTS
-- ----------------------------------------------------------------------------

handleMenuEvent :: Event -> GameState -> IO GameState
handleMenuEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state = do
  case () of
    _ | hitDiffButton Easy mx my -> 
          return $ selectDifficulty Easy state
      | hitDiffButton Medium mx my ->
          return $ selectDifficulty Medium state
      | hitDiffButton Hard mx my ->
          return $ selectDifficulty Hard state
      | hitDiffButton Expert mx my ->
          return $ selectDifficulty Expert state
      | hitStartButton mx my -> do
          -- Generate puzzle
          let diff = gsSelectedDiff state
          puzzle <- generatePuzzle diff
          case resolverSudoku puzzle of
            Nothing -> return $ state { gsMessage = "Failed to generate puzzle, try again" }
            Just solution -> do
              time <- getCurrentTime
              return $ newGame diff puzzle solution time
      | otherwise -> return state

handleMenuEvent _ state = return state

-- ----------------------------------------------------------------------------
-- GAME EVENTS
-- ----------------------------------------------------------------------------

handleGameEvent :: Event -> GameState -> IO GameState
handleGameEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state@GameState{..} = do
  case () of
    -- Cell selection
    _ | Just coord <- hitBoard mx (my - gridYOffset) -> do
             if gsWaitingForCell
               then case requestHint gsSelectedHint coord state of
                 Left err -> return $ state { gsMessage = err, gsWaitingForCell = False }
                 Right newState -> checkCompletion newState
               else return $ state { gsSelectedCell = Just coord }
    
      -- Hint button clicks
      | hitHintButton Parity mx my ->
          return $ state { gsSelectedHint = Parity, gsWaitingForCell = True
                         , gsMessage = "Select a cell to get PARITY hint" }
      | hitHintButton Prime mx my ->
          return $ state { gsSelectedHint = Prime, gsWaitingForCell = True
                         , gsMessage = "Select a cell to get PRIME hint" }
      | hitHintButton Divisible3 mx my ->
          return $ state { gsSelectedHint = Divisible3, gsWaitingForCell = True
                         , gsMessage = "Select a cell to get DIVISIBILITY hint" }
      | hitHintButton NarrowRange mx my ->
          return $ state { gsSelectedHint = NarrowRange, gsWaitingForCell = True
                         , gsMessage = "Select a cell to get RANGE hint" }
      | hitHintButton Reveal mx my ->
          return $ state { gsSelectedHint = Reveal, gsWaitingForCell = True
                         , gsMessage = "Select a cell to REVEAL answer" }
    
      -- Control buttons
      | hitCheckButton mx my ->
          if isComplete gsBoard && isValidBoard gsBoard
          then checkCompletion state
          else return $ state { gsMessage = "Puzzle not complete or has errors" }
    
      | hitSolveButton mx my ->
          return $ state { gsBoard = gsSolution, gsMessage = "Puzzle solved!" }
    
      | hitResetButton mx my ->
          return $ resetGame state
    
      | otherwise -> return state

-- Number key input
handleGameEvent (EventKey (Char c) Down _ _) state@GameState{..}
  | isDigit c, Just coord <- gsSelectedCell =
      let value = ord c - ord '0'
      in case placeCellValue coord value state of
           Left err -> return $ state { gsMessage = err }
           Right newState -> checkCompletion newState
  | otherwise = return state

handleGameEvent _ state = return state

-- ----------------------------------------------------------------------------
-- VICTORY EVENTS
-- ----------------------------------------------------------------------------

handleVictoryEvent :: Event -> GameState -> IO GameState
handleVictoryEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) state
  | hitPlayAgainButton mx my = return $ resetGame state
  | otherwise = return state
handleVictoryEvent _ state = return state

-- ============================================================================
-- HIT DETECTION
-- ============================================================================

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

-- Difficulty buttons (menu)
hitDiffButton :: Difficulty -> Float -> Float -> Bool
hitDiffButton diff = inButton x (-30)
  where
    x = case diff of
      Easy -> -150
      Medium -> 0
      Hard -> 150
      Expert -> 0

hitStartButton :: Float -> Float -> Bool
hitStartButton = inButton 0 (-210)

-- Hint buttons (left panel)
hitHintButton :: HintType -> Float -> Float -> Bool
hitHintButton hType mx my = inButton cx cy mx my
  where
    cx = -gridSize/2 - 70
    cy = 200 + case hType of
      Parity      -> -40
      Prime       -> -90
      Divisible3  -> -140
      NarrowRange -> -190
      Reveal      -> -240
      _           -> 0

-- Control buttons (bottom)
hitCheckButton, hitSolveButton, hitResetButton :: Float -> Float -> Bool
hitCheckButton = inButton (-130) (-halfGrid - 20)
hitSolveButton = inButton 0 (-halfGrid - 20)
hitResetButton = inButton 130 (-halfGrid - 20)

-- Victory button
hitPlayAgainButton :: Float -> Float -> Bool
hitPlayAgainButton = inButton 0 (-180)

-- Generic button hit detection
inButton :: Float -> Float -> Float -> Float -> Bool
inButton cx cy mx my =
  mx >= cx - buttonWidth/2 &&
  mx <= cx + buttonWidth/2 &&
  my >= cy - buttonHeight/2 &&
  my <= cy + buttonHeight/2
