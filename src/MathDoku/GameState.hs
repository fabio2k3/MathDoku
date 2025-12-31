{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

{-|
Module      : MathDoku.GameState
Description : Game state management and logic for MathDoku
Copyright   : (c) Your Team, 2025
License     : MIT
Maintainer  : your-team@example.com
Stability   : stable

Manages complete game state including board, score, hints, and game flow.
Implements state machine pattern for menu/playing/completed states.

Based on:
- Keera Studios Haskell Game Programming: State machine patterns
- Monday Morning Haskell: Gloss game state management
- Haskell GameDev: Starting with Gloss best practices
-}
module MathDoku.GameState
    ( -- * Game State Types
        GameState(..)
    , GamePhase(..)
    , Statistics(..)
    
    -- * State Creation
    , initialState
    , newGame
    
    -- * Game Actions
    , selectDifficulty
    , placeCellValue
    , requestHint
    , checkCompletion
    , resetGame
    
    -- * Queries
    , canAffordHint
    , isGameComplete
    , getCurrentScore
    ) where

-- ============================================================================
-- IMPORTS
-- ============================================================================
import MathDoku.Core
import MathDoku.Hints
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)

-- ============================================================================
-- GAME PHASE (State Machine)
-- ============================================================================

-- | Game phases following state machine pattern
--
-- State transitions:
-- SelectDifficulty -> (select) -> GeneratingPuzzle -> Playing
-- Playing -> (complete) -> GameWon
-- Any -> (reset) -> SelectDifficulty
data GamePhase
    = SelectDifficulty              -- ^ Menu: choosing difficulty
    | GeneratingPuzzle Difficulty   -- ^ Loading: generating puzzle
    | Playing                       -- ^ Active gameplay
    | GameWon NominalDiffTime       -- ^ Victory screen with elapsed time
    deriving (Show, Eq)

-- ============================================================================
-- GAME STATE (Complete World State)
-- ============================================================================

-- | Complete game state
--
-- Following Gloss best practices: store ALL state in one record
-- > "Describe your state in a top-level world type"
-- > - Haskell GameDev Guide
data GameState = GameState
    { -- Board state
        gsBoard :: Board               -- ^ Current board (with user moves)
    , gsOriginal :: Board            -- ^ Original puzzle (Fixed cells)
    , gsSolution :: Board            -- ^ Complete solution (for hints)
    
        -- Game flow
    , gsPhase :: GamePhase           -- ^ Current phase in state machine
    , gsSelectedDiff :: Difficulty   -- ^ Difficulty for new game
    
        -- Scoring and resources
    , gsPoints :: Int                -- ^ Available points for hints
    , gsInitialPoints :: Int         -- ^ Starting points (depends on difficulty)
    
        -- Gameplay tracking
    , gsMovesCount :: Int            -- ^ Total moves made
    , gsHintsUsed :: [(HintType, Coord)] -- ^ History of hints used
    , gsStartTime :: Maybe UTCTime   -- ^ When gameplay started
    
        -- UI state
    , gsSelectedCell :: Maybe Coord  -- ^ Currently selected cell
    , gsSelectedHint :: HintType     -- ^ Active hint type selection
    , gsWaitingForCell :: Bool       -- ^ Waiting for cell selection after hint button
    , gsMessage :: String            -- ^ Current message to display
    
        -- Statistics
    , gsStats :: Statistics          -- ^ Detailed game statistics
    } deriving (Show)

-- ============================================================================
-- STATISTICS
-- ============================================================================

-- | Detailed game statistics for end screen
data Statistics = Statistics
    { statCellsFilled :: Int         -- ^ Cells filled correctly
    , statErrors :: Int              -- ^ Invalid moves attempted
    , statHintsUsed :: Int           -- ^ Total hints purchased
    , statParityUsed :: Int
    , statPrimeUsed :: Int
    , statDivisibleUsed :: Int
    , statRangeUsed :: Int
    , statRevealUsed :: Int
    } deriving (Show, Eq)

-- | Empty statistics
emptyStats :: Statistics
emptyStats = Statistics 0 0 0 0 0 0 0 0

-- ============================================================================
-- STATE CREATION
-- ============================================================================

-- | Initial state (menu screen)
initialState :: GameState
initialState = GameState
    { gsBoard = emptyBoard
    , gsOriginal = emptyBoard
    , gsSolution = emptyBoard
    , gsPhase = SelectDifficulty
    , gsSelectedDiff = Medium
    , gsPoints = 0
    , gsInitialPoints = 0
    , gsMovesCount = 0
    , gsHintsUsed = []
    , gsStartTime = Nothing
    , gsSelectedCell = Nothing
    , gsSelectedHint = Parity
    , gsWaitingForCell = False
    , gsMessage = "Select difficulty and press START"
    , gsStats = emptyStats
    }

-- | Create new game with generated puzzle
--
-- Called after puzzle generation completes
newGame :: Difficulty -> Board -> Board -> UTCTime -> GameState
newGame diff original solution time = GameState
    { gsBoard = original
    , gsOriginal = original
    , gsSolution = solution
    , gsPhase = Playing
    , gsSelectedDiff = diff
    , gsPoints = initialPoints diff
    , gsInitialPoints = initialPoints diff
    , gsMovesCount = 0
    , gsHintsUsed = []
    , gsStartTime = Just time
    , gsSelectedCell = Nothing
    , gsSelectedHint = Parity
    , gsWaitingForCell = False
    , gsMessage = "Solve the puzzle! Use hints wisely."
    , gsStats = emptyStats
    }
    where
        -- Points awarded based on difficulty
        initialPoints Easy = 15
        initialPoints Medium = 12
        initialPoints Hard = 10
        initialPoints Expert = 8

-- ============================================================================
-- GAME ACTIONS
-- ============================================================================

-- | Select difficulty in menu
selectDifficulty :: Difficulty -> GameState -> GameState
selectDifficulty diff state@GameState{gsPhase = SelectDifficulty} =
    state 
        { gsSelectedDiff = diff
        , gsMessage = "Difficulty: " ++ show diff ++ ". Press START to play."
        }
selectDifficulty _ state = state  -- Ignore if not in menu


-- | Place value in cell
placeCellValue :: Coord -> Int -> GameState -> Either String GameState
placeCellValue coord@(r, c) value state@GameState{..}
    -- Validate phase
    | gsPhase /= Playing = 
        Left "Cannot place values outside of gameplay"
    -- Validate cell is modifiable
    | not (isCellEmpty coord gsOriginal) =
        Left "Cannot modify original puzzle cells"
    -- Validate move according to Sudoku rules
    | not (isValidMove coord value gsBoard) =
        Right $ state { gsStats = gsStats { statErrors = statErrors gsStats + 1 }
                      , gsMessage = "Invalid move! Violates Sudoku rules."
                      }
    -- Valid move: place value and check correctness
    | otherwise =
        let newBoard  = placeValue coord value gsBoard
            correct   = cellValue coord gsSolution == value
            newStats  = if correct
                           then gsStats { statCellsFilled = statCellsFilled gsStats + 1 }
                           else gsStats { statErrors = statErrors gsStats + 1 }
        in Right $ state
             { gsBoard        = newBoard
             , gsMovesCount   = gsMovesCount + 1
             , gsSelectedCell = Nothing
             , gsStats        = newStats
             , gsMessage      = if correct then "Correct! ✓" else "Incorrect value for this cell ✗"
             }


-- | Request hint for selected cell
requestHint :: HintType -> Coord -> GameState -> Either String GameState
requestHint hintType coord state@GameState{gsSolution = sol, gsBoard, gsPoints, gsHintsUsed, gsStats, gsWaitingForCell, ..}
    | gsPhase /= Playing = Left "Hints only during gameplay"
    | gsPoints < hintCost hintType = Left $ "Need " ++ show (hintCost hintType)
    | otherwise =
        case getHint hintType sol coord of
            Left err -> Left $ case err of
                CellAlreadyFilled _ -> "Cannot hint on filled cells"
                CellOutOfBounds _ -> "Invalid cell"
                PuzzleHasNoSolution -> "Puzzle error"
                InvalidHintType _ -> "Bad hint"
            Right hint ->
                let newPoints  = gsPoints - hintCost hintType
                    newBoard   = if hintType == Reveal then placeValue coord (hintValue hint) gsBoard else gsBoard
                    newStats   = updateHintStats hintType gsStats
                in Right $ state
                    { gsBoard      = newBoard
                    , gsPoints     = newPoints
                    , gsStats      = newStats
                    , gsHintsUsed  = (hintType, coord) : gsHintsUsed
                    , gsMessage    = hintMessage hint
                    , gsWaitingForCell = False
                    }

-- | Update statistics based on hint type used
updateHintStats :: HintType -> Statistics -> Statistics
updateHintStats hType stats =
    let stats' = stats { statHintsUsed = statHintsUsed stats + 1 }
    in case hType of
        Parity      -> stats' { statParityUsed = statParityUsed stats' + 1 }
        Prime       -> stats' { statPrimeUsed = statPrimeUsed stats' + 1 }
        Divisible3  -> stats' { statDivisibleUsed = statDivisibleUsed stats' + 1 }
        Divisible2  -> stats' { statDivisibleUsed = statDivisibleUsed stats' + 1 }
        NarrowRange -> stats' { statRangeUsed = statRangeUsed stats' + 1 }
        MediumRange -> stats' { statRangeUsed = statRangeUsed stats' + 1 }
        Reveal      -> stats' { statRevealUsed = statRevealUsed stats' + 1 }

-- | Check if puzzle is complete and transition to GameWon
checkCompletion :: GameState -> IO GameState
checkCompletion state@GameState{..}
    | gsPhase /= Playing = return state
    | not (isComplete gsBoard) = return state
    | not (isValidBoard gsBoard) = return $ state 
            { gsMessage = "Board has errors! Please fix them." }
    | otherwise = do
            currentTime <- getCurrentTime
            let elapsed = case gsStartTime of
                        Nothing -> 0
                        Just start -> diffUTCTime currentTime start
            return $ state
                { gsPhase = GameWon elapsed
                , gsMessage = "🎉 Congratulations! Puzzle solved!"
                }

-- | Reset game to menu
resetGame :: GameState -> GameState
resetGame _ = initialState

-- ============================================================================
-- QUERIES
-- ============================================================================

-- | Check if player can afford a hint
canAffordHint :: HintType -> GameState -> Bool
canAffordHint hType GameState{..} = gsPoints >= hintCost hType

-- | Check if game is complete
isGameComplete :: GameState -> Bool
isGameComplete GameState{..} = case gsPhase of
    GameWon _ -> True
    _ -> isComplete gsBoard && isValidBoard gsBoard


-- | Get current score
getCurrentScore :: GameState -> Int
getCurrentScore GameState{..} =
    let baseScore        = gsPoints
        difficultyBonus  = case gsSelectedDiff of
            Easy    -> 10
            Medium  -> 25
            Hard    -> 50
            Expert  -> 100
        errorPenalty     = statErrors gsStats * 5
    in max 0 (baseScore + difficultyBonus - errorPenalty)
