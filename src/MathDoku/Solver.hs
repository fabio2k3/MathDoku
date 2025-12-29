{-# LANGUAGE ScopedTypeVariables #-}

module MathDoku.Solver
  ( -- * Public API
    resolverSudoku
    -- * Internal utilities (for testing)
  , emptyPositions
  ) where

import MathDoku.Core
import Data.List ((\\))
import Data.Maybe (mapMaybe)

-- ============================================================================
-- MAIN SOLVER
-- ============================================================================

-- | Solve complete Sudoku using optimized backtracking
--
--   Algorithm:
--   1. If complete, validate and return
--   2. Find next empty cell
--   3. Try valid candidates recursively
resolverSudoku :: Board -> Maybe Board
resolverSudoku board
  | isComplete board = if isValidBoard board then Just board else Nothing
  | otherwise =
      case emptyPositions board of
        [] -> Nothing
        (pos:rest) -> solveStep board (pos:rest)

-- ============================================================================
-- BACKTRACKING CORE
-- ============================================================================

-- | Recursive step: solve from specific position
solveStep :: Board -> [Coord] -> Maybe Board
solveStep board [] = Just board
solveStep board (pos:rest) =
  let candidates = getCandidates board pos
  in tryCandidates board pos candidates rest

-- | Valid candidates for position (reuses Core)
getCandidates :: Board -> Coord -> [Int]
getCandidates board pos =
  [n | n <- [1..9], isValidMove pos n board]

-- | Try candidates until valid solution found
tryCandidates :: Board -> Coord -> [Int] -> [Coord] -> Maybe Board
tryCandidates _ _ [] _ = Nothing
tryCandidates board pos (n:ns) rest =
  let newBoard = placeValue pos n board
  in case solveStep newBoard rest of
       Just solution -> Just solution
       Nothing -> tryCandidates board pos ns rest

-- ============================================================================
-- POSITION UTILITIES
-- ============================================================================

-- | Return coordinates of all empty cells using list comprehension.
--   Identical to Generator for maximum compatibility.
emptyPositions :: Board -> [Coord]
emptyPositions board =
  [(r, c) | r <- [0..8], c <- [0..8], isCellEmpty (r, c) board]

-- ============================================================================
-- OPTIMIZATIONS (optional - enable for hard puzzles)
-- ============================================================================

-- | Naked singles: fill cells with only one possible candidate
nakedSingles :: Board -> Maybe Board
nakedSingles board =
  let singles = [ (pos, n) | pos <- emptyPositions board
                          , let cands = getCandidates board pos
                          , [n] <- [cands] ]
  in if null singles then Nothing
     else Just (foldl (\b (p,v) -> placeValue p v b) board singles)