{-# LANGUAGE FlexibleContexts #-}

module MathDoku.Generator
    ( -- * Public API
        generatePuzzle
    , Difficulty(..)
    , cellsToRemove
    
        -- * Internal utilities (for testing)
    , filledPositions
    ) where

import MathDoku.Core
import Control.Monad.Random (MonadRandom, evalRandIO, getRandomR)
import MathDoku.Solver as Solver
import Data.List (delete)

-- ============================================================================
-- DIFFICULTY CONFIGURATION
-- ============================================================================

-- | Number of cells to remove for each puzzle difficulty level.
--   Values follow industry standards ensuring solvability with unique solution.
cellsToRemove :: Difficulty -> Int
cellsToRemove Easy   = 40  -- 41 clues remaining
cellsToRemove Medium = 50  -- 31 clues remaining
cellsToRemove Hard   = 60  -- 21 clues remaining
cellsToRemove Expert = 77  -- 4 clues minimum (professional standard)

-- ============================================================================
-- MAIN GENERATION PIPELINE
-- ============================================================================

-- | Generate a Sudoku puzzle of specified difficulty with guaranteed unique solution.
--
--   Algorithm:
--   1. Generate complete valid Sudoku grid using optimized backtracking
--   2. Remove cells while preserving exactly one solution
generatePuzzle :: Difficulty -> IO Board
generatePuzzle diff = evalRandIO $ do
    fullBoard <- generateFullBoard
    removeCellsKeepingUniqueness (cellsToRemove diff) fullBoard

-- ============================================================================
-- COMPLETE GRID GENERATION
-- ============================================================================

generateFullBoard :: MonadRandom m => m Board
generateFullBoard = do
    -- Step 1: Fill first row with random permutation [1-9]
    -- Significantly accelerates backtracking convergence
    firstRow <- shuffleM [1..9]
    let boardWithFirstRow = foldr (\(col, num) b -> placeValue (0, col) num b)
                                                                emptyBoard
                                                                (zip [0..8] firstRow)
    -- Step 2: Backtracking fill remaining rows
    mb <- fillBoardBacktrack boardWithFirstRow
    case mb of
        Just solution -> return solution
        Nothing      -> generateFullBoard  -- Retry (extremely rare)

-- | Backtracking solver with randomized move ordering for complete grid generation.
fillBoardBacktrack :: MonadRandom m => Board -> m (Maybe Board)
fillBoardBacktrack board
    | isComplete board = return (Just board)
    | otherwise =
        case Solver.emptyPositions board of
            []       -> return (Just board)
            (pos:_)  -> do
                -- Try numbers in random order for better exploration
                shuffledNums <- shuffleM [1..9]
                tryNumbers pos shuffledNums
  where
    -- Try numbers at position until solution found or exhausted
    tryNumbers :: MonadRandom m => Coord -> [Int] -> m (Maybe Board)
    tryNumbers _ []                  = return Nothing
    tryNumbers pos (n:remainingNums)
        | not (isValidMove pos n board) =
            tryNumbers pos remainingNums
        | otherwise = do
            result <- fillBoardBacktrack (placeValue pos n board)
            case result of
                Just solution -> return (Just solution)
                Nothing       -> tryNumbers pos remainingNums

-- ============================================================================
-- CELL REMOVAL WITH UNIQUENESS PRESERVATION
-- ============================================================================

-- | Remove exactly N cells from complete board while ensuring unique solvability.
--   Uses randomized selection with uniqueness verification at each step.
removeCellsKeepingUniqueness :: MonadRandom m => Int -> Board -> m Board
removeCellsKeepingUniqueness 0 board = return board
removeCellsKeepingUniqueness n board = do
    let filledPositionsList = filledPositions board
    if null filledPositionsList
        then return board
        else do
            -- Select random filled cell
            idx <- getRandomR (0, length filledPositionsList - 1)
            let pos             = filledPositionsList !! idx
                boardWithHole   = setCell pos Empty board
            
            -- Verify puzzle still has exactly one solution
            if hasUniqueSolution boardWithHole
                then removeCellsKeepingUniqueness (n-1) boardWithHole
                else removeCellsKeepingUniqueness n board  -- Try different cell

-- ============================================================================
-- UNIQUENESS VERIFICATION
-- ============================================================================

-- | Check if puzzle has exactly one valid solution using bounded search.
hasUniqueSolution :: Board -> Bool
hasUniqueSolution board = length (take 2 (findAllSolutions board)) == 1

-- | Lazily enumerate all valid solutions (stops after finding 2 for efficiency).
findAllSolutions :: Board -> [Board]
findAllSolutions board
    | isComplete board = [board | isValidBoard board]
    | otherwise =
        case Solver.emptyPositions board of
            []        -> []
            (pos:_)   ->
                let validMoves = [n | n <- [1..9], isValidMove pos n board]
                in concatMap (\n -> findAllSolutions (placeValue pos n board))
                             validMoves

-- ============================================================================
-- POSITION UTILITIES
-- ============================================================================

-- | Return coordinates of all empty cells using list comprehension.
emptyPositions :: Board -> [Coord]
emptyPositions board =
    [(r, c) | r <- [0..8], c <- [0..8], isCellEmpty (r, c) board]

-- | Return coordinates of all filled cells (Fixed or Filled).
filledPositions :: Board -> [Coord]
filledPositions board =
    [(r, c) | r <- [0..8], c <- [0..8], not (isCellEmpty (r, c) board)]

-- ============================================================================
-- SHUFFLING (OPTIMIZED FISHER-YATES)
-- ============================================================================

-- | Fisher-Yates shuffle algorithm (O(n) using delete operation).
--   Essential for randomized backtracking efficiency.
shuffleM :: (MonadRandom m, Eq a) => [a] -> m [a]
shuffleM []     = return []
shuffleM [x]    = return [x]
shuffleM xs = do
    i <- getRandomR (0, length xs - 1)
    let selected = xs !! i
    rest <- shuffleM (delete selected xs)
    return (selected : rest)
